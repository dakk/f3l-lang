type iden = string
[@@deriving show {with_path = false}]

type ttype = 
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

let attributes (t: ttype) = match t with 
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

let pp_ttype fmt (t: ttype) = Format.pp_print_string fmt (show_ttype t); ()

let compare t1 t2 = t1 = t2

let compare_lazy t t' = match t', t with 
  | TPair(a, TAny), TPair (c, b) -> a = c
  | TPair(a, b), TPair (c, TAny) -> a = c
  | TAny, _ -> true
  | _, TAny -> true
  | a, b -> a = b

let compare_list t1 t2 = 
  if List.length t1 = 1 && List.length t2 == 0 && List.hd t1 = TUnit then true 
  else List.length (List.filter (fun (a,b) -> a<>b) @@ List.combine t1 t2) = 0

