open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Translate_ptype

let show_ttype_got_expect t1 t2 = "got: '" ^ show_ttype t1 ^ "' expect '" ^ show_ttype t2 ^ "'"
let show_ttype_got t1 = "got: '" ^ show_ttype t1 ^ "'"


type iref = 
| Local of ttype
[@@deriving show {with_path = false}]

type ireft = 
| ILocal
| IAny

type bindings = (iden * iref) list
[@@deriving show {with_path = false}]

let binding_find ic (st: ireft) i: iref option = 
  match (List.find_opt (fun (a, b) -> 
    match st, b with 
    | IAny, _ when i=a -> true
    | ILocal, Local(_) when i=a -> true
    | _, _ -> false
  ) ic) with 
  | Some(_,b) -> Some(b)
  | None -> None

(* transform an pexpr to (ttype * expr) *)
let rec transform_expr (pe: Parse_tree.pexpr) (env': Env.t) (ic: bindings) : texpr = 
  let push_ic i ii ic = (i, ii)::(List.remove_assoc i ic) in
  let pel = Pt_loc.eline pe in
  let r = (match pe with

  (* Literals *)
  | PEUnit -> TUnit, Unit
  | PEString (s) -> TString, String (s)
  | PEBytes (s) -> TBytes, Bytes (Bytes.of_string s)
  | PEFloat (f) -> TFloat, Float (f)
  | PEInt (n) -> TInt, Int (n)
  | PEBool (b) -> TBool, Bool (b)

  (* Composed types *)
  | PEPair (e1, e2) -> 
    let te1 = transform_expr e1 env' ic in
    let te2 = transform_expr e2 env' ic in
    TPair(fst te1, fst te2), Pair(te1, te2)

  | PETyped (e, et) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let tt' = transform_type et env' in
    (match tt, tt', ee with 
    | a, b, _ when compare_lazy a b -> a, ee
    | a, b, c -> raise @@ TypeError (pel, "Invalid cast from '" ^ show_ttype a ^ "' to '" ^ show_ttype b ^ "' for value: " ^ show_expr c))

  | PELambda ((argi, argt), e) -> 
    let argt = transform_type argt env' in
    let (tt, ee) = transform_expr e env' (push_ic argi (Local(argt)) ic) in 
    TLambda (argt, tt), Lambda((argi, argt), (tt, ee))
   

  (* symbol reference *)    
  | PERef (i) -> 
    (match binding_find ic ILocal i with 
    | None -> (
      let ref = Env.get_ref i env' in 
      match List.assoc_opt i env'.symbols with
      | Some(Union) -> ref, UnionValue (i)
      | Some(External) -> ref, External (i, ref)
      | _ -> ref, GlobalRef (i)
    )
    | Some (Local(t)) -> t, LocalRef (i)
    )


  (* native bool function *)
  | PEApply (PERef(nf), PEPair(e1, e2)) when nf = "&&" || nf = "||" -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let a,b = tt1 |> type_final, tt2 |> type_final in
    if a <> TBool && a <> TAny || b <> TBool && b <> TAny then
      raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got_expect (TPair(tt1, tt2)) @@ TPair(TBool, TBool));
    (match nf with 
    | "&&" -> TBool, And((tt1, ee1), (tt2, ee2))
    | "||" -> TBool, Or((tt1, ee1), (tt2, ee2))
    | _ -> failwith "unreachable")

  | PEApply (PERef(nf), e1) when nf = "not" -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let a = tt1 |> type_final in
    if a <> TBool && a <> TAny then
      raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got_expect tt1 @@ TBool);
    TBool, Not((tt1, ee1))

  (* equality *)
  | PEApply (PERef(nf), PEPair(e1, e2)) when nf = "<>" || nf = "="-> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let a, b = tt1 |> type_final, tt2 |> type_final in
    if not (compare_lazy a b) then
      raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got (TPair(tt1, tt2)));
    (match nf with 
    | "=" -> TBool, Eq((tt1, ee1), (tt2, ee2))
    | "<>" -> TBool, Neq((tt1, ee1), (tt2, ee2))
    | _ -> failwith "unreachable")


  (* native comparison function *)
  | PEApply (PERef(nf), PEPair(e1, e2)) when nf = ">=" || nf = "<=" || nf = ">" || nf = "<" || nf = "<>" || nf = "="-> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let a, b = tt1 |> type_final, tt2 |> type_final in
    if (a = TInt && b = TFloat || a = TFloat && b = TInt) || (a <> TInt && a <> TFloat && a <> TAny || b <> TInt && b <> TFloat && b <> TAny) then
      raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got (TPair(tt1, tt2)));
    (match nf with 
    | ">=" -> TBool, Gte((tt1, ee1), (tt2, ee2))
    | ">" -> TBool, Gt((tt1, ee1), (tt2, ee2))
    | "<=" -> TBool, Lte((tt1, ee1), (tt2, ee2))
    | "<" -> TBool, Lt((tt1, ee1), (tt2, ee2))
    | "=" -> TBool, Eq((tt1, ee1), (tt2, ee2))
    | "<>" -> TBool, Neq((tt1, ee1), (tt2, ee2))
    | _ -> failwith "unreachable")

  (* native int function *)
  | PEApply (PERef(nf), PEPair(e1, e2)) when nf = "+" || nf = "-" || nf = "/" || nf = "*" || nf = "mod" -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let a,b = tt1 |> type_final, tt2 |> type_final in
    if a <> TInt && a <> TAny || b <> TInt && b <> TAny then
      raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got_expect (TPair(tt1, tt2)) @@ TPair(TInt, TInt));
    (match nf with 
    | "+" -> TInt, Add((tt1, ee1), (tt2, ee2))
    | "-" -> TInt, Sub((tt1, ee1), (tt2, ee2))
    | "/" -> TInt, Div((tt1, ee1), (tt2, ee2))
    | "*" -> TInt, Mul((tt1, ee1), (tt2, ee2))
    | "mod" -> TInt, Mod((tt1, ee1), (tt2, ee2))
    | _ -> failwith "unreachable")

  (* native float function *)
  | PEApply (PERef(nf), PEPair(e1, e2)) when nf = "+." || nf = "-." || nf = "/." || nf = "*." -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let a,b = tt1 |> type_final, tt2 |> type_final in
    if a <> TFloat && a <> TAny || b <> TFloat && b <> TAny then
      raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got_expect (TPair(tt1, tt2)) @@ TPair(TFloat, TFloat));
    (match nf with 
    | "+." -> TFloat, FAdd((tt1, ee1), (tt2, ee2))
    | "-." -> TFloat, FSub((tt1, ee1), (tt2, ee2))
    | "/." -> TFloat, FDiv((tt1, ee1), (tt2, ee2))
    | "*." -> TFloat, FMul((tt1, ee1), (tt2, ee2))
    | _ -> failwith "unreachable")

  (* native pair/list function *)
  | PEApply (PERef(nf), c) when nf = "fst" || nf = "snd" || nf = "hd" || nf = "tl" -> 
    let (tt1, ee1) = transform_expr c env' ic in 
    let pres = (tt1, ee1) in 
    (match (tt1 |> type_final), nf with 
    | TPair(a, _), "fst" 
    | TPair(a, _), "hd" -> a, PairFst (pres) 
    | TPair(_, b), "snd"
    | TPair(_, b), "tl" -> b, PairSnd (pres)
    | TAny, "fst" 
    | TAny, "hd" -> TAny, PairFst (pres)
    | TAny, "snd"
    | TAny, "tl" -> TAny, PairSnd (pres)
    | _ -> raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got_expect tt1 @@ TPair(TAny, TAny)))
    

  (* other apply case *)
  | PEApply (e, c) -> 
    let (tt,ee) = transform_expr e env' ic in  
    (match tt |> type_final with
    | TAny -> TAny, Apply((tt, ee), transform_expr c env' ic)
    | TLambda (arg, rettype) -> 
      let ap = transform_expr c env' ic in
      if not @@ Ast_ttype.compare_lazy arg (fst ap) then 
        raise @@ TypeError (pel, "Invalid argument types apply; " ^ show_ttype_got_expect arg (fst ap))
      else
        rettype, Apply((tt, ee), ap)

    | _ -> raise @@ TypeError (pel, "Applying on not a lambda: " ^ show_ttype tt)
  )
  

  | PEIfThenElse (c, e1, e2) -> 
    let (tc, ec) = transform_expr c env' ic in 
    let (te1, ee1) = transform_expr e1 env' ic in 
    let (te2, ee2) = transform_expr e2 env' ic in 
    (match tc |> type_final, te1 |> type_final, te2 |> type_final with 
    | TBool, t, t' when compare_lazy t t' -> t, IfThenElse ((tc, ec), (te1, ee1), (te2, ee2))
    | TBool, t, t' when not (compare_lazy t t') -> 
      raise @@ TypeError (pel, "If branches should have same type, got: '" ^ show_ttype t ^ "' and '" ^ show_ttype t' ^ "'")
    | _, _, _ -> raise @@ TypeError (pel, "If condition should be a boolean expression, got '" ^ show_ttype tc ^ "'"))



  | PELetIn(i, top, e, e1, recursive) -> 
    let (tt, ee) = 
      if recursive then transform_expr e env' ((i, Local (TLambda(TAny, TAny)))::ic)
      else transform_expr e env' ic in 
    let t' = match top with | None -> tt | Some(t) -> transform_type t env' in
    if not @@ compare_lazy tt t' then raise @@ TypeError (pel, "LetIn type mismatch; " ^ show_ttype_got_expect tt t');
    let (tt1, ee1) = transform_expr e1 env' @@ push_ic i (Local(t')) ic in 
    tt1, LetIn (i, t', (tt, ee), (tt1, ee1))

  ) in 
match pel with 
| Some(p, _, _, _) -> Ast_loc.loce p r
| None -> r
