open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Translate_ptype

let show_ttype_got_expect t1 t2 = "got: '" ^ show_ttype t1 ^ "' expect '" ^ show_ttype t2 ^ "'"
let show_ttype_between_na t1 t2 = "between '" ^ show_ttype t1 ^ "' and '" ^ show_ttype t2 ^ "' is not allowed"
let show_ttype_not_cmp t1 t2 = "Types '" ^ show_ttype t1 ^ "' and '" ^ show_ttype t2 ^ "' are not comparable"


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
  let assert_comparable tt1 tt2 = 
    if tt1 <> tt2 then raise @@ TypeError (pel, show_ttype_not_cmp tt1 tt2);
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> ()
    | _, _ -> raise @@ TypeError (pel, show_ttype_not_cmp tt1 tt2))
  in
  let r = (match pe with

  (* Literals *)
  | PEUnit -> TUnit, Unit
  | PEString (s) -> TString, String (s)
  | PEBytes (s) -> TBytes, Bytes (Bytes.of_string s)
  | PENat (n) -> TNat, Nat (n)
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
    | a, b, _ when a=b -> a, ee
    | a, b, c -> raise @@ TypeError (pel, "Invalid cast from '" ^ show_ttype a ^ "' to '" ^ show_ttype b ^ "' for value: " ^ show_expr c))

  | PELambda ((argi, argt), e) -> 
    let argt = transform_type argt env' in
    let (tt, ee) = transform_expr e env' (push_ic argi (Local(argt)) ic) in 
    TLambda (argt, tt), Lambda((argi, argt), (tt, ee))

  | PERecord (l) -> 
    let l' = List.map (fun (i, p) -> i, transform_expr p env' ic) l in 
    let (idtt, _) = List.map (fun (i, (tt, ee)) -> (i, tt), (i, ee)) l' |> List.split in
    TRecord (idtt), Record (l')


  (* PEDot record access *)
  | PEDot (e, i) -> 
    let (te, ee) = transform_expr e env' ic in
    (match te with 
    | TRecord(t) -> 
      (match List.assoc_opt i t with 
        | None -> raise @@ TypeError (pel, "Unkown record field '" ^ i ^ "'")
        | Some(t) -> t, RecordAccess((te, ee), i))
    | _ -> raise @@ InvalidExpression (pel, "Unhandled dot access of '" ^ i ^ "' on expression '" ^ show_expr ee ^ "'")
    )



  (* Arithmetic *)
  | PEAdd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1 |> type_final, tt2 |> type_final with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TFloat, TFloat -> TFloat
      | _, _ -> raise @@ TypeError (pel, "Add " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Add ((tt1, ee1), (tt2, ee2))

  | PEMul (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1 |> type_final, tt2 |> type_final with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TFloat, TFloat -> TFloat
      | _, _ -> raise @@ TypeError (pel, "Mul " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Mul ((tt1, ee1), (tt2, ee2))


  | PEMod (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1 |> type_final, tt2 |> type_final with 
      | TNat, TNat -> TPair(TNat, TNat)
      | TNat, TInt -> TPair(TInt, TNat)
      | TInt, TNat -> TPair(TInt, TNat)
      | TInt, TInt -> TPair(TInt, TNat)
      | _, _ -> raise @@ TypeError (pel, "Div " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Mod ((tt1, ee1), (tt2, ee2))

  | PEDiv (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1 |> type_final, tt2 |> type_final with 
      | TNat, TNat -> TInt
      | TNat, TInt -> TInt
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | _, _ -> raise @@ TypeError (pel, "Div " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Div ((tt1, ee1), (tt2, ee2))


  | PESub (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1 |> type_final, tt2 |> type_final with 
      | TNat, TNat -> TInt
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | _, _ -> raise @@ TypeError (pel, "Sub " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Sub ((tt1, ee1), (tt2, ee2))


  (* Boolean *)
  | PENot (e1) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    if tt1 |> type_final = TBool then TBool, Not (tt1, ee1) 
    else raise @@ TypeError (pel, "Not needs a boolean expression, got: '" ^ show_ttype tt1 ^ "'")

  | PEOr (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    if tt1 |> type_final <> TBool || tt2 |> type_final <> TBool then 
      raise @@ TypeError (pel, "Or branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'");
    TBool, Or ((tt1, ee1), (tt2, ee2))

  | PEAnd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    if tt1 |> type_final <> TBool || tt2 |> type_final <> TBool then 
      raise @@ TypeError (pel, "And branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'");
    TBool, And ((tt1, ee1), (tt2, ee2))
  
  (* Compare *)
  | PEGt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    assert_comparable tt1 tt2;
    TBool, Gt((tt1, ee1), (tt2, ee2))

  | PEGte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    assert_comparable tt1 tt2;
    TBool, Gte((tt1, ee1), (tt2, ee2))
    
  | PELt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    assert_comparable tt1 tt2;
    TBool, Lt((tt1, ee1), (tt2, ee2))

  | PELte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    assert_comparable tt1 tt2;
    TBool, Lte((tt1, ee1), (tt2, ee2))

  | PEEq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    assert_comparable tt1 tt2;
    TBool, Eq((tt1, ee1), (tt2, ee2))

  | PENeq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    assert_comparable tt1 tt2;
    TBool, Neq((tt1, ee1), (tt2, ee2))
    

  (* symbol reference *)    
  | PERef (i) -> 
    (match binding_find ic ILocal i with 
    | None -> let ref = Env.get_ref i env' in ref, GlobalRef (i)
    | Some (Local(t)) -> t, LocalRef (i)
    )

  (* native pair function *)
  | PEApply (PERef(nf), c) when nf = "fst" || nf = "snd" -> 
    let (tt1, ee1) = transform_expr c env' ic in 
    let pres = (tt1, ee1) in 
    (match tt1 |> type_final with 
    | TPair(a, b) -> if nf = "fst" then a, PairFst (pres) else b, PairSnd (pres)
    | TAny -> TPair(TAny, TAny), if nf = "fst" then PairFst (pres) else PairSnd (pres)
    | _ -> raise @@ TypeError (pel, nf ^ " wrong exp passed; " ^ show_ttype_got_expect tt1 @@ TPair(TAny, TAny)))
    
      

  | PEApply (e, c) -> 
    let (tt,ee) = transform_expr e env' ic in  
    (match tt |> type_final with
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
    | TBool, t, t' when t = t' -> t, IfThenElse ((tc, ec), (te1, ee1), (te2, ee2))
    | TBool, t, t' when t <> t' -> 
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
