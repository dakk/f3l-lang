type iden = string
[@@deriving show {with_path = false}]

type ttype = 
  | TTypeRef of iden * ttype
  | TAny
  | TUnit
  | TInt
  | TNat
  | TFloat
  | TBool
  | TString
  | TBytes
  | TLambda of ttype * ttype
  | TUnion of string list
  | TPair of ttype * ttype
  (* | TCont of iden * ttype   *)

let is_base (t: ttype) = match t with 
  | TUnit -> true
  | TAny -> true
  | TInt -> true
  | TNat -> true
  | TFloat -> true
  | TBool -> true
  | TString -> true
  | TBytes -> true 
  | _ -> false

let rec type_final tt1: ttype = match tt1 with
| TTypeRef(_, t) -> type_final t 
| TPair(t1, t2) -> TPair ((type_final t1), (type_final t2))
| TLambda (tt1, tt2) -> TLambda (type_final tt1, type_final tt2)
(* | TCont(i, t) -> TCont(i, type_final t) *)
| _ -> tt1

  

let rec show_ttype (at: ttype) = match at with 
| TAny -> "'a"
| TUnit -> "unit"
| TInt -> "int"
| TNat -> "nat"
| TFloat -> "float"
| TBool -> "bool"
| TString -> "string"
| TBytes -> "bytes"
| TLambda (p, r) -> "(" ^ show_ttype p ^ " -> " ^ show_ttype r ^ ")"
| TUnion (el) -> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " | ") ^ x) "" el
| TPair (t1, t2) -> "(" ^ show_ttype t1 ^ " * " ^ show_ttype t2 ^ ")"
| TTypeRef (i, _) -> i

let pp_ttype fmt (t: ttype) = Format.pp_print_string fmt (show_ttype t); ()

let rec comparable a b = match a, b with 
| TAny, _ 
| _, TAny -> true 
| TPair(a, b), TPair(c, d) -> comparable a c && comparable b d
| a, b when a = b -> true 
| _ -> false

let compare_strict t t' = t = t'

let rec compare_lazy t t' = match t' |> type_final, t |> type_final with 
  | TPair(a, b), TPair(TAny, TAny)
  | TPair(TAny, TAny), TPair(a, b) -> a = b
  | TPair(a, b), TPair(c, d) -> compare_lazy a c && compare_lazy b d
  | TLambda (a, b), TLambda (c, d) -> compare_lazy a c && compare_lazy b d
  | TAny, _ -> true
  | _, TAny -> true
  | a, b -> a = b


let rec allowed_cast f t = match f, t with 
| a, b when a = b -> true
| TNat, TInt -> true
| TAny, _ -> true
| _, TAny -> true
| TPair(a, b), TPair(TAny, TAny) -> a = b
| TPair(a, b), TPair(c, d) -> allowed_cast a c && allowed_cast b d
| TLambda (a, b), TLambda (c, d) -> allowed_cast a c && allowed_cast b d
| _, _ -> false