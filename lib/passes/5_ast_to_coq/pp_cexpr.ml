open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Format
open Helpers.Gen_utils
open Big_int
open Pp_ctype

let pp_ltype t a = ()

let pp_par fmt ((ti, tt): string * ttype) = 
  fprintf fmt "%s: %a" ti pp_ctype tt


let rec pp_cexpr fmt ((te,e): texpr) = 
  let let_surround fmt s = fprintf fmt "let %s = @[%a@] in " (temp_v ()) pp_cexpr s in
  let pp_infix2 fmt op a b = fprintf fmt "(%a) %s (%a)" pp_cexpr a op pp_cexpr b in
    
  match e with
  (* 
  | ContractInstance of expr 
  
  | BuildContractCodeAndStorage of iden * expr list
  
  | Entrypoint of expr * iden
  
  *)
  
  
  (* 
  | GlobalRef (id)
  | LocalRef (id) -> 
    fprintf fmt "%s" id
  
  | StorageRef (id) -> 
    fprintf fmt "s.%s" id
  
  
  | None -> 
    fprintf fmt "None"  
  *)

  | Unit -> 
    fprintf fmt "unit"

  | Bool (i) -> 
    (match i with 
    | true -> fprintf fmt "True"
    | false -> fprintf fmt "False")

  | Nat (i) -> 
    fprintf fmt "%s" @@ string_of_big_int i

  | Int (i) -> 
    fprintf fmt "%s" @@ string_of_big_int i

  (*  
  | String (s) -> 
    fprintf fmt "\"%s\"" s
  
  | Some(a) -> 
    fprintf fmt "Some (%a)" pp_cexpr a
  
  | Bytes (s) -> 
    fprintf fmt "(\"%s\": bytes)" (Bytes.to_string s)
  
  | EnumValue (i) -> 
    (match te with | TEnum(e) -> fprintf fmt "%dn" @@ enum_index e i 0)
  
  | Typed (e, t) -> 
    fprintf fmt "(%a: %a)" 
      pp_cexpr e
      pp_ltype t
    
  | List (el) -> 
    fprintf fmt "[ %a ]" (pp_list "; " pp_cexpr) el
  
  
  | Tuple (el) -> 
    fprintf fmt "( %a )" (pp_list ", " pp_cexpr) el
  
  | Lambda (il, e) -> 
    if List.length il = 0 then 
      fprintf fmt "( fun (override: unit) -> @[@\n%a@] )" pp_cexpr e 
    else 
      fprintf fmt "(fun (%a) -> @[%a@])"    
        pp_mpar il
        pp_cexpr e
  
  | Record (il) -> 
    let pp_rec_as fmt (i, e) = fprintf fmt "%s=%a" i pp_cexpr e in
    fprintf fmt "{ %a }"
      (pp_list "; " pp_rec_as) il
    
  | RecordAccess (e, i) -> 
    fprintf fmt "%a.%s" 
      pp_cexpr e 
      i *)
  
  (* option *)
  (* | OptionGetSome (oe) -> 
    fprintf fmt "(match (%a) with | Some(v) -> v | None -> failwith \"Expect some value\")" 
      pp_cexpr oe
  
  | OptionIsSome(oe) -> 
    fprintf fmt "(match (%a) with | Some(v) -> true | None -> false)" 
      pp_cexpr oe
  
  | OptionIsNone(oe) -> 
    fprintf fmt "(match (%a) with | Some(v) -> true | None -> true)" 
      pp_cexpr oe *)
  
  
  
  
  (* list *)
  | ListEmpty -> 
    fprintf fmt "[]"
  
  | ListMapWith (le, ll) -> 
    fprintf fmt "List.map (%a) (%a)" 
      pp_cexpr ll
      pp_cexpr le
  
  | ListPrepend (le, el) -> 
    fprintf fmt "(%a) :: (%a)"
      pp_cexpr el 
      pp_cexpr le
  
  | ListSize (le) ->
    fprintf fmt "List.size (%a)" pp_cexpr le
  
  | ListFold (le, ll, initial) -> 
    fprintf fmt "List.fold (%a) (%a) (%a)" 
      pp_cexpr ll
      pp_cexpr le
      pp_cexpr initial
  
  | ListFilter ((TList(lt), le), ll) ->
    fprintf fmt "List.fold (fun (acc, e: %a * %a) -> if (%a)(e) then e::acc else acc) (%a) ([]: %a)"
      pp_ltype (TList(lt))
      pp_ltype (lt)
      pp_cexpr ll
      pp_cexpr (TList(lt), le)
      pp_ltype (TList(lt))
  
  (*
  | ListHead of expr
  | ListTail of expr
  
  (* string *)
  | StringSlice of expr * expr * expr
  *)
  (* | StringSize (s) -> 
    fprintf fmt "String.length (%a)" pp_cexpr s
  
  | StringConcat (a, b) -> 
    pp_infix2 fmt "^" a b *)
  
  (* bytes *)
  (* | BytesPack(a) -> 
    fprintf fmt "Bytes.pack (%a)" pp_cexpr a
  
  | BytesUnpack(a) -> 
    fprintf fmt "Bytes.unpack (%a)" pp_cexpr a
  
  | BytesSize (s) -> 
    fprintf fmt "Bytes.length (%a)" pp_cexpr s *)
  
  (* BytesSlice(a,b,c) *)
  
  (* | BytesConcat (a, b) -> 
    pp_infix2 fmt "^" a b
   *)
  
  (* tuple *)
  (*
  | TupleFst of expr
  | TupleSnd of expr
  *)
  
  (* aritmetic *) 
  (* | Add(a,b) -> 
    pp_infix2 fmt "+" a b
    
  | Sub(a,b) -> 
    pp_infix2 fmt "-" a b
    
  | Mul(a,b) -> 
    pp_infix2 fmt "*" a b
    
  | Div(a,b) -> 
    pp_infix2 fmt "/" a b
    
  | Abs(a) -> 
    fprintf fmt "abs(%a)" pp_cexpr a
  
  | ToInt(a) -> 
    fprintf fmt "int(%a)" pp_cexpr a
  
  | IsNat(a) -> 
    fprintf fmt "Michelson.is_nat(%a)" pp_cexpr a
  
  | Neg(a) -> 
    fprintf fmt "- (%a)" pp_cexpr a
  
  | Mod (a, b) -> 
    pp_infix2 fmt "mod" a b
   *)
  
  (* bool *)
  (* | Not(a) -> 
    fprintf fmt "! (%a)" pp_cexpr a
  
  | And(a,b) -> 
    pp_infix2 fmt "&&" a b
    
  | Or(a,b) -> 
    pp_infix2 fmt "||" a b
  
  | Lt (a, b) -> 
    pp_infix2 fmt "<" a b
    
  | Lte (a, b) -> 
    pp_infix2 fmt "<=" a b
  
  | Gt (a, b) -> 
    pp_infix2 fmt ">" a b
  
  | Gte (a, b) -> 
    pp_infix2 fmt ">=" a b
  
  | Eq (a, b) -> 
    pp_infix2 fmt "=" a b
  
  | Neq (a, b) -> 
    pp_infix2 fmt "<>" a b
  
  | IfThenElse (c, a, b) -> 
    fprintf fmt "(if %a then %a else %a)" 
      pp_cexpr c
      pp_cexpr a 
      pp_cexpr b
   
  | Apply(lam, par) -> 
    fprintf fmt "%a (%a)" 
      pp_cexpr lam 
      pp_cexpr par
  
  | MatchWith (e, el) -> 
    let rec rr fmt el = (match el with 
    | [] -> fprintf fmt ""
    | (e', te')::((_, CaseDefault), tee')::el' -> 
      fprintf fmt "if %s = (%a) then @[(%a: %a)@]@\nelse (%a: %a)"
        "tmatchwithtemp"
        pp_cexpr e'
        pp_cexpr te'
        pp_ltype te
        pp_cexpr tee'
        pp_ltype te
  
    | (e', te')::elle::el' -> 
      fprintf fmt "if %s = (%a) then @[(%a: %a)@]@\nelse %a" 
        "tmatchwithtemp"
        pp_cexpr e'
        pp_cexpr te'
        pp_ltype te
        rr (elle::el')
  
    | (e', te')::[] -> 
      fprintf fmt "if %s = (%a) then @[(%a: %a)@] " 
        "tmatchwithtemp"
        pp_cexpr e'
        pp_cexpr te'
        pp_ltype te
    ) in 
    fprintf fmt "let %s = %a in @\n%a" 
      "tmatchwithtemp"
      pp_cexpr e
      rr el
  
    
  | Copy (e) -> 
    fprintf fmt "(%a)" pp_cexpr e
       
  | Let (id, tt, e) -> 
    fprintf fmt "let %s: %a = %a in " 
      id 
      pp_ltype tt
      pp_cexpr e
  
  | LetIn (id, tt, e, e2) -> 
    fprintf fmt "let %s: %a = @\n%a in @\n%a " 
      id 
      pp_ltype tt
      pp_cexpr e 
      pp_cexpr e2
  
  | LetTuple (il, e) -> 
    fprintf fmt "let (%a) = %a in "
      (pp_list ", " pp_str) (fst @@ List.split il)
      pp_cexpr e 
  
  | LetTupleIn (il, e, e2) -> 
    fprintf fmt "let (%a) = %a in @,%a"
      (pp_list ", " pp_str) (fst @@ List.split il)
      pp_cexpr e 
      pp_cexpr e2
  
  | SAssign (i, e) -> 
    fprintf fmt "let s = { s with %s=%a } in " 
      i 
      pp_cexpr e
  
  | SRecAssign (i, ii, expr) -> 
    fprintf fmt "let s = { s with %s= {s.%s with %s=%a} } in " 
      i i ii pp_cexpr expr
   *)

  | Seq(a, b) -> 
    fprintf fmt "@,%a@,%a"
  
    (match a with 
    | (TUnit, LetTuple(_, _)) 
    | (TUnit, LetTupleIn(_, _, _))
    | (TUnit, LetIn(_, _, _, _))
    | (TUnit, Let(_, _, _)) 
    | (TUnit, SAssign(_, _)) 
    | (TUnit, SRecAssign(_, _, _)) -> pp_cexpr
    | (TUnit, _) -> let_surround
    | _ -> pp_cexpr)
    a
    
    (match b with 
    | (tl, List(e)) -> (fun fmt un -> fprintf fmt "(%a: operation list)" pp_cexpr (tl, List(e)))
    | _ -> (fun fmt un -> fprintf fmt "%a" pp_cexpr b))
    ()

  | _ -> fprintf fmt "<<%s>>" (show_expr e)