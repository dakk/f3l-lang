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
  | TUnion of (string * ttype) list
  | TList of ttype
  | TOption of ttype
  | TRecord of (iden * ttype) list
  | TTuple of ttype list 


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
  | TList (_) ->      { cmp=false; pack=true  }
  | TOption (_) ->    { cmp=false; pack=true  }
  | TRecord (_) ->    { cmp=false; pack=true  } 
  | TTuple (_) ->     { cmp=true;  pack=true  }
 
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
| TUnion (el) -> List.fold_left (fun acc (x, _) -> acc ^ (if acc = "" then "" else " | ") ^ x) "" el
| TList (t) -> show_ttype t ^ " list"
| TOption (t) -> show_ttype t ^ " option"
| TRecord (l) -> "record { " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else ", ") ^ x ^ ": " ^ show_ttype xt) "" l ^ " }"
| TTuple (tl) -> "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ show_ttype x) "" tl ^ ")"

let pp_ttype fmt (t: ttype) = Format.pp_print_string fmt (show_ttype t); ()

let compare t1 t2 = t1 = t2

let compare_type_lazy t t' = (match t', t with 
  | TList(_), TList(TAny) -> true 
  | a, b when a=b -> true
  | _, _ -> false
) 

let compare_list t1 t2 = 
  if List.length t1 = 1 && List.length t2 == 0 && List.hd t1 = TUnit then true 
  else List.length (List.filter (fun (a,b) -> a<>b) @@ List.combine t1 t2) = 0

