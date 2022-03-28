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
  | TRecord of (iden * ttype) list
  | TPair of ttype * ttype

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
| TTypeRef(tn, t) -> type_final t 
| TPair(t1, t2) -> TPair ((type_final t1), (type_final t2))
| TRecord(tl) -> TRecord (List.map (fun (n, t) -> (n, type_final t)) tl)
| TLambda (tt1, tt2) -> TLambda (type_final tt1, type_final tt2)
| _ -> tt1


type tattr = {
  cmp   : bool;
  pack  : bool;
} [@@deriving show {with_path = false}]

(* 
  Comparable
      Comparable values can be stored in sets, can be passed as argument to COMPARE, etc.
  Packable
      Values of packable types can be given as serialized using the PACK primitive.
*)

let attributes (t: ttype) = match t |> type_final with 
  | TUnit ->          { cmp=false; pack=true  }
  | TInt ->           { cmp=true;  pack=true  }
  | TNat ->           { cmp=true;  pack=true  }
  | TFloat ->         { cmp=true;  pack=true  }
  | TBool ->          { cmp=true;  pack=true  }
  | TString ->        { cmp=true;  pack=true  }
  | TBytes ->         { cmp=true;  pack=true  }
  | TLambda (_, _) -> { cmp=false; pack=true  }
  | TUnion (_) ->     { cmp=true;  pack=true  } 
  | TRecord (_) ->    { cmp=false; pack=true  } 
  | TPair (_, _) ->   { cmp=true;  pack=true  } 
  | TTypeRef (_, _) ->{ cmp=true;  pack=true  }
 
  (* internal types *)
  | TAny ->           { cmp=false; pack=false }
  

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
| TRecord (l) -> "record { " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else ", ") ^ x ^ ": " ^ show_ttype xt) "" l ^ " }"
| TPair (t1, t2) -> "(" ^ show_ttype t1 ^ " * " ^ show_ttype t2 ^ ")"
| TTypeRef (i, t) -> i

let pp_ttype fmt (t: ttype) = Format.pp_print_string fmt (show_ttype t); ()



let compare t1 t2 = (t1 |> type_final) = (t2 |> type_final)

let compare_lazy t t' = match t' |> type_final, t |> type_final with 
  | TPair(a, TAny), TPair (c, _) -> a = c
  | TPair(a, _), TPair (c, TAny) -> a = c
  | TAny, _ -> true
  | _, TAny -> true
  | a, b -> a = b
