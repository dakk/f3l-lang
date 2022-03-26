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
  | Some(a,b) -> Some(b)
  | None -> None

(* transform an pexpr to (ttype * expr) *)
let rec transform_expr (pe: Parse_tree.pexpr) (env': Env.t) (ic: bindings) : texpr = 
  let argv_to_list pel = match pel with | TTuple(tl) -> tl | _ -> [pel] in
  let transform_expr_list pel = List.map (fun p -> transform_expr p env' ic) pel in
  let transform_iexpr_list pel = List.map (fun (i, p) -> i, transform_expr p env' ic) pel in
  let transform_itype_list pel = List.map (fun (i, p) -> i, transform_type p env') pel in
  let push_ic i ii ic = (i, ii)::(List.remove_assoc i ic) in
  let push_local_many rl ic = List.fold_left (fun ic (i,x) -> (i, Local(x))::(List.remove_assoc i ic)) ic rl in
  let pel = Pt_loc.eline pe in
  let assert_comparable tt1 tt2 = 
    if tt1 <> tt2 then raise @@ TypeError (pel, show_ttype_not_cmp tt1 tt2);
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> ()
    | _, _ -> raise @@ TypeError (pel, show_ttype_not_cmp tt1 tt2))
  in
  let fold_container_type debs l =
    List.fold_left (fun acc xt -> if acc <> xt then 
      raise @@ TypeError (pel, debs ^ " must have the same type: " ^ show_ttype acc ^ " <> " ^ show_ttype xt)
    else 
      xt
    ) (List.hd l) l
  in
  let r = (match pe with
  (* Option *)
  | PENone -> TOption (TAny), None
  | PESome (e) -> 
    let (tt, te) = transform_expr e env' ic in
    TOption (tt), Some (tt, te)

  (* Literals *)
  | PEUnit -> TUnit, Unit
  | PEString (s) -> TString, String (s)
  | PEBytes (s) -> TBytes, Bytes (Bytes.of_string s)
  | PENat (n) -> TNat, Nat (n)
  | PEFloat (f) -> TFloat, Float (f)
  | PEInt (n) -> TInt, Int (n)
  | PEBool (b) -> TBool, Bool (b)

  (* Composed types *)
  | PETuple (el) -> 
    let tel = el |> transform_expr_list in
    TTuple(fst @@ List.split tel), Tuple(tel)

  | PEList (el) -> 
    let ttres = el |> transform_expr_list in
    let (ttl, tel) = ttres |> List.split in
    if List.length ttl > 0 then 
      let lt = fold_container_type "List elements" ttl in TList(lt), List(ttres)
    else 
      TList(TAny), List([])

  | PETyped (e, et) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let tt' = transform_type et env' in
    (match tt, tt', ee with 
    | TOption (TAny), TOption(t), None -> TOption(t), None
    | TOption (TAny), TOption(t), be -> TOption(t), be
    | TList (TAny), TList(t), ee -> TList(t), ee
    | a, b, _ when a=b -> a, ee
    | a, b, c -> raise @@ TypeError (pel, "Invalid cast from '" ^ show_ttype a ^ "' to '" ^ show_ttype b ^ "' for value: " ^ show_expr c))

  | PELambda (argl, e) -> 
    let rl = argl |> transform_itype_list in
    let (tt, ee) = transform_expr e env' (push_local_many rl ic) in 
    let arg = (match List.length rl with 
      | 0 -> TUnit
      | 1 -> snd @@ List.hd rl
      | _ -> TTuple (snd @@ List.split rl)
    ) in
    TLambda (arg, tt), Lambda(rl, (tt, ee))

  | PERecord (l) -> 
    let l' = l |> transform_iexpr_list in 
    let (idtt, idee) = List.map (fun (i, (tt, ee)) -> (i, tt), (i, ee)) l' |> List.split in
    TRecord (idtt), Record (l')

    
  (* Enum value *)
  | PEHt (ii, i) -> 
    (match Env.get_type_opt ii env' with 
    | Some(TEnum (el)) -> 
      if List.find_opt (fun x -> x=i) el <> None then
        TEnum(el), EnumValue(i)
      else 
        raise @@ TypeError (pel, "Enum value '" ^ i ^ "' not found in enum: " ^ show_ttype (TEnum(el)))
    | None -> raise @@ TypeError (pel, "Unknown enum type '" ^ ii ^ "'")
    | _ -> raise @@ TypeError (pel, "Accessor # is only usable on enum type"))



  (* PEDot on base *)
  | PEApply (PEDot (PERef("List"), "empty"), []) -> TList(TAny), ListEmpty
    

  | PEApply (PEDot (PERef("Bytes"), "pack"), c) -> 
    if List.length c <> 1 then raise @@ APIError (pel, "Bytes.pack needs only one argument");
    let (tt1, ee1) = transform_expr (List.hd c) env' ic in 
    (* TODO: check for pack attribute *)
    TBytes, BytesPack((tt1, ee1))
  | PEApply (PEDot (PERef("Bytes"), "unpack"), c) -> 
    if List.length c <> 1 then raise @@ APIError (pel, "Bytes.unpack needs only one arguments");
    let (tt2, ee2) = transform_expr (List.hd c) env' ic in 
    if tt2 <> TBytes then  raise @@ TypeError (pel, "unpack needs a bytes expression, got: " ^ show_ttype tt2);
    TOption(TAny), BytesUnpack((tt2, ee2))


  (* PEApply(PEDot) base type apis *)
  | PEApply (PEDot(e,i), el) -> 
    let (te, ee) = transform_expr e env' ic in
    let el' = el |> transform_expr_list in 
    (match ee, te, i, el' with 
      | _, TOption (ts), "getSome", [] -> ts, OptionGetSome(te, ee)
      | _, TOption (ts), "isSome", [] -> TBool, OptionIsSome(te, ee)
      | _, TOption (ts), "isNone", [] -> TBool, OptionIsNone(te, ee)

      (* List *)
      | _, TList (_), "size", [] -> TNat, ListSize (te, ee)
      | _, TList (l), "head", [] -> l, ListHead (te, ee)
      | _, TList (l), "tail", [] -> TList (l), ListTail (te, ee)
      | _, TList (l), "fold", [(TLambda (TTuple([ll; rt']), rt), lame); (ft, ff)] when l=ll && rt=rt' && rt=ft -> 
        ft, ListFold((te, ee), (TLambda (TTuple([ll; rt']), rt), lame), (ft, ff))

      | _, TList (l), "prepend", [(ll, e)] when ll = l -> 
        TList (l), ListPrepend ((te, ee), (ll, e))

      | _, TList (l), "mapWith", [(TLambda (ll, rt), lame)] when l = ll -> 
        TList (rt), ListMapWith ((te, ee), (TLambda (ll, rt), lame))

      | _, TList (l), "filter", [(TLambda (ll, TBool), lame)] when l=ll -> 
        TList (l), ListFilter((te, ee), (TLambda (ll, TBool), lame))


      (* String *)
      | _, TString, "slice", [(TInt, i1); (TInt, i2)] -> TString, StringSlice ((te, ee), (TNat, i1), (TNat, i2))
      | _, TString, "size", [] -> TNat, StringSize(te, ee)

      (* Bytes *)
      | _, TBytes, "slice", [(TInt, i1); (TInt, i2)] -> TBytes, BytesSlice ((te, ee), (TNat, i1), (TNat, i2))
      | _, TBytes, "size", [] -> TNat, BytesSize(te, ee)

      (* Tuple *)
      | _, TTuple ([a; _]), "fst", [] -> a, TupleFst (te, ee)
      | _, TTuple ([_; b]), "snd", [] -> b, TupleSnd (te, ee)


      | _, _, i, _-> 
        raise @@ TypeError (pel, "Invalid apply of " ^ i ^ " over '" ^ show_ttype te ^ "'")
    )

  (* PEDot *)
  | PEDot (e, i) -> 
    let (te, ee) = transform_expr e env' ic in
    (match te with 
    (* PEDot record access *)
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
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TFloat, TFloat -> TFloat
      | TString, TString -> TString 
      | TBytes, TBytes -> TBytes 
      | _, _ -> raise @@ TypeError (pel, "Add " ^ show_ttype_between_na tt1 tt2)
    ) in
    if tt1 = TString then rt, StringConcat ((tt1, ee1), (tt2, ee2)) else 
    if tt1 = TBytes then rt, BytesConcat ((tt1, ee1), (tt2, ee2))
    else rt, Add ((tt1, ee1), (tt2, ee2))

  | PEMul (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TFloat, TFloat -> TFloat
      | _, _ -> raise @@ TypeError (pel, "Mul " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Mul ((tt1, ee1), (tt2, ee2))

  | PEEDiv (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TOption(TTuple([TNat; TNat]))
      | TNat, TInt -> TOption(TTuple([TInt; TNat])) 
      | TInt, TNat -> TOption(TTuple([TInt; TNat])) 
      | TInt, TInt -> TOption(TTuple([TInt; TNat])) 
      | _, _ -> raise @@ TypeError (pel, "EDiv " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, EDiv ((tt1, ee1), (tt2, ee2))

  | PEMod (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TOption(TTuple([TNat; TNat]))
      | TNat, TInt -> TOption(TTuple([TInt; TNat])) 
      | TInt, TNat -> TOption(TTuple([TInt; TNat])) 
      | TInt, TInt -> TOption(TTuple([TInt; TNat])) 
      | _, _ -> raise @@ TypeError (pel, "Div " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Mod ((tt1, ee1), (tt2, ee2))

  | PEDiv (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
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
    let rt = (match tt1, tt2 with 
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
    if tt1 = TBool then TBool, Not (tt1, ee1) 
    else raise @@ TypeError (pel, "Not needs a boolean expression, got: '" ^ show_ttype tt1 ^ "'")

  | PEOr (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    if tt1 <> TBool || tt2 <> TBool then 
      raise @@ TypeError (pel, "Or branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'");
    TBool, Or ((tt1, ee1), (tt2, ee2))

  | PEAnd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    if tt1 <> TBool || tt2 <> TBool then 
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
    | None -> Env.get_ref i env', GlobalRef (i)
    | Some (Local(t)) -> t, LocalRef (i)
    | _ -> raise @@ SymbolNotFound (pel, "Symbol '" ^ i ^ "' is not a valid ref")
    )

  (* apply *)
  | PEApply (PERef("abs"), c) -> 
    if List.length c <> 1 then raise @@ APIError (pel, "abs needs only one argument");
    let (tt1, ee1) = transform_expr (List.hd c) env' ic in 
    if tt1 <> TInt then raise @@ TypeError (pel, "Abs " ^ show_ttype_between_na tt1 TInt);
    TNat, Abs ((tt1, ee1))

  | PEApply (PERef("neg"), c) ->
    if List.length c <> 1 then raise @@ APIError (pel, "neg needs only one argument");
    let (tt1, ee1) = transform_expr (List.hd c) env' ic in 
    (match tt1 with 
    | TInt -> TInt, Neg((tt1,ee1))
    | TNat -> TInt, Neg((tt1,ee1))
    | _ -> raise @@ TypeError (pel, "neg needs an int or a nat, got: " ^ show_ttype tt1))

  | PEApply (PERef("int"), c) -> 
    if List.length c <> 1 then raise @@ APIError (pel, "int needs only one argument");
    let (tt1, ee1) = transform_expr (List.hd c) env' ic in 
    if tt1 <> TNat then raise @@ TypeError (pel, "Int " ^ show_ttype_between_na tt1 TNat);
    TInt, ToInt ((tt1, ee1))

  | PEApply (PERef("isNat"), c) -> 
    if List.length c <> 1 then raise @@ APIError (pel, "isNat needs only one argument");
    let (tt1, ee1) = transform_expr (List.hd c) env' ic in 
    if tt1 <> TInt then raise @@ TypeError (pel, "isNat " ^ show_ttype_between_na tt1 TNat);
    TBool, ToInt ((tt1, ee1))


  | PEApply (e, el) -> 
    let (tt,ee) = transform_expr e env' ic in 
    (match tt with 
    (* Apply on lambda  *)
    | TLambda (arv, rettype) -> 
      let argl = argv_to_list arv in 
      let ap = el |> transform_expr_list in
      if not (List.length ap = 0 && List.length argl = 1 && List.hd argl = TUnit) && List.length argl <> List.length ap then 
        raise @@ InvalidExpression (pel, "Invalid argument number for lambda apply");
      if not @@ Ast_ttype.compare_list argl (fst @@ List.split ap) then 
        raise @@ TypeError (pel, "Invalid argument types apply");
      if List.length argl = 1 && List.hd argl = TUnit then 
        rettype, Apply ((tt,ee), (TUnit, Unit))
      else if List.length argl > 1 then 
        rettype, Apply((tt, ee), (TTuple(argl), Tuple(ap)))
      else if (List.length ap) = 0 && (List.length argl) = 0 then
        rettype, Apply((tt, ee), (TUnit, Unit))
      else
        rettype, Apply((tt, ee), List.hd ap)

      
    | _ -> raise @@ TypeError (pel, "Applying on not a lambda: " ^ show_ttype tt)
    
    (* (Parse_tree.show_pexpr (PEApply(e, el)))) *)
  )
  

  | PEIfThenElse (c, e1, e2) -> 
    let (tc, ec) = transform_expr c env' ic in 
    let (te1, ee1) = transform_expr e1 env' ic in 
    let (te2, ee2) = transform_expr e2 env' ic in 
    (match tc, te1, te2 with 
    | TBool, TList(t), TList(t') when t <> t' && (t <> TAny || t' <> TAny) ->
      (if t <> TAny then TList(t) else TList(t')), IfThenElse ((tc, ec), (te1, ee1), (te2, ee2))
    | TBool, t, t' when t = t' -> t, IfThenElse ((tc, ec), (te1, ee1), (te2, ee2))
    | TBool, t, t' when t <> t' -> 
      raise @@ TypeError (pel, "If branches should have same type, got: '" ^ show_ttype t ^ "' and '" ^ show_ttype t' ^ "'")
    | _, _, _ -> raise @@ TypeError (pel, "If condition should be a boolean expression, got '" ^ show_ttype tc ^ "'"))

  | PEMatchWith (e, bl) -> 
    let (te, ee) = transform_expr e env' ic in 
    let bl' = List.map (fun (cv, cex)  -> 
      let (tt, ee) = transform_expr cv env' ic in
      let (tcex, ecex) = transform_expr cex env' ic in
      if (tt <> te) && (tt <> TAny) then
        raise @@ TypeError (pel, "Match case has an invalid value type; " ^ show_ttype_got_expect tt te)
      else ((tt, ee), tcex, (tcex, ecex)) 
    ) bl in
    (* assert that every branch as the same type *)
    let rett: ttype = List.fold_left (fun acc (_, tcex, _) -> 
      if acc <> tcex && tcex <> TUnit then  (* TODO: tany is only allowed for fail *)
        raise @@ TypeError (pel, "Match branches should have same type; " ^ show_ttype_got_expect tcex acc)
      else if tcex = TUnit then acc else tcex
    ) (let (_,b,_) = List.hd bl' in b) bl'
    in rett, MatchWith ((te, ee), List.map (fun (a,_,c) -> (a,c)) bl')

  | PECaseDefault -> TAny, CaseDefault


  (* let-binding and sequences *)
  | PELetIn(i, top, e, e1) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let t' = match top with | None -> tt | Some(t) -> transform_type t env' in
    if not @@ compare_type_lazy tt t' then raise @@ TypeError (pel, "LetIn type mismatch; " ^ show_ttype_got_expect tt t');
    let (tt1, ee1) = transform_expr e1 env' @@ push_ic i (Local(t')) ic in 
    tt1, LetIn (i, t', (tt, ee), (tt1, ee1))

  | PESeq(PELet(i, top, e), en) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let t' = match top with | None -> tt | Some(t) -> transform_type t env' in
    if not @@ compare_type_lazy tt t' then raise @@ TypeError (pel, "Let type mismatch; " ^ show_ttype_got_expect tt t'); 
    let (tnt, ene) = transform_expr en env' @@ push_ic i (Local(t')) ic in
    tnt, Seq((TUnit, Let(i, t', (tt, ee))), (tnt, ene))

  | PELetTuple(tl, e) -> 
    let (tt, ee) = transform_expr e env' ic in 
    (* TODO optional types of tl are ignored! *)
    let ti = fst @@ List.split tl in
    (match tt with 
      | TTuple(tl') -> tt, LetTuple(List.combine ti tl', (tt, ee))
      | _ -> raise @@ TypeError (pel, "Expected a tuple")
    )

  | PELetTupleIn(tl, e, e1) -> 
    let (tt, ee) = transform_expr e env' ic in 
    (* TODO optional types of tl are ignored! *)
    let ti = fst @@ List.split tl in
    (match tt with 
      | TTuple(tl') -> 
        let tl' = List.combine ti tl' in 
        let (tt1, ee1) = transform_expr e1 env' @@ push_local_many tl' ic in 
        tt1, LetTupleIn(tl', (tt, ee), (tt1, ee1))
      | _ -> raise @@ TypeError (pel, "Expected a tuple")
    )

  | PESeq(PELetTuple(tl, e), en) -> 
    let (tt, ee) = transform_expr e env' ic in 
    (* TODO optional types of tl are ignored! *)
    let ti = fst @@ List.split tl in
    (match tt with 
      | TTuple(tl') -> 
        let tl' = List.combine ti tl' in 
        let (tnt, ene) = transform_expr en env' @@ push_local_many tl' ic in
        tnt, Seq((TUnit, LetTuple(tl', (tt, ee))), (tnt, ene))
      | _ -> raise @@ TypeError (pel, "Expected a tuple")
    )

  | PESeq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    if tt1 <> TUnit then raise @@ InvalidExpression (pel, "Cannot ignore non unit expression in sequence");
    (tt2, Seq((tt1, ee1), (tt2, ee2)))

(* 
    
  | Parse_tree.PEType (dt) :: p' -> 
    Env.assert_symbol_absence e dt.id;

    transform p' { e with 
      symbols=(dt.id, Type)::e.symbols;
      types=(dt.id, transform_type dt.t e)::e.types;
    }
    *)
  | _ -> (TUnit, Unit)


  
  | ex -> raise @@ InvalidExpression (pel, "Expression not handled yet: " ^ Parse_tree.show_pexpr ex)
  ) in 
match pel with 
| Some(p, _, _, _) -> Ast_loc.loce p r
| None -> r
