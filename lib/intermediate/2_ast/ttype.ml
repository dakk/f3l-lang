type iden = string
[@@deriving show {with_path = false}]

type ttype = 
  | TAny
  | TUnit
  | TInt
  | TNat
  | TBool
  | TString
  | TBytes
  | TLambda of ttype * ttype
  | TEnum of string list
  | TList of ttype
  | TOption of ttype
  | TRecord of (iden * ttype) list
  | TTuple of ttype list 
  | TContract of ttype 

  (* custom abstract types *)
  | TContractCode of (iden * ttype list) list
  | TContractStorage
  | TInterface of (iden * ttype list) list
  | TContractInstance of ttype


type tattr = {
  push  : bool;
  cmp   : bool;
  pass  : bool;
  store : bool;
  pack  : bool;
  bm_val: bool;
} [@@deriving show {with_path = false}]

(* 
  Comparable
      Comparable values can be stored in sets, can be passed as argument to COMPARE, etc.
  Passable
      Passable types are those that can be taken as a parameter in contracts.
  Storable
      Storable types can be used as a storage in contracts.
  Pushable
      Literal values of pushable types can be given as parameter to the PUSH primitive.
  Packable
      Values of packable types can be given as serialized using the PACK primitive.
  big_map value
      These are types that be used in the domain of big_maps. 
*)

let attributes (t: ttype) = match t with 
  | TUnit ->          { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TInt ->           { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TNat ->           { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TBool ->          { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TString ->        { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TBytes ->         { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TLambda (_, _) -> { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TEnum (_) ->      { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  } 
  | TList (_) ->      { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TOption (_) ->    { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TRecord (_) ->    { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  } 
  | TTuple (_) ->     { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TContract (_) ->  { cmp=false; pass=true;  store=false; push=false; pack=true;  bm_val=true  }
 
  (* internal types *)
  | TAny ->           { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false }
  | TContractCode (_) ->  
                      { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false }
  | TContractStorage ->  
                      { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false }
  | TInterface (_) ->     
                      { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false }
  | TContractInstance (_) -> 
                      { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false }


let rec show_ttype (at: ttype) = match at with 
| TAny -> "'a"
| TUnit -> "unit"
| TInt -> "int"
| TNat -> "nat"
| TBool -> "bool"
| TString -> "string"
| TBytes -> "bytes"
| TLambda (p, r) -> "(" ^ show_ttype p ^ " -> " ^ show_ttype r ^ ")"
| TEnum (el) -> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " | ") ^ x) "" el
| TList (t) -> show_ttype t ^ " list"
| TOption (t) -> show_ttype t ^ " option"
| TRecord (l) -> "record { " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else ", ") ^ x ^ ": " ^ show_ttype xt) "" l ^ " }"
| TTuple (tl) -> "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ show_ttype x) "" tl ^ ")"
| TContract (t) -> show_ttype t ^ " contract"

| TContractCode (_) -> "code"
| TContractStorage -> "storage"
| TInterface (_) -> "interface"
| TContractInstance (_) -> "instance"

let pp_ttype fmt (t: ttype) = Format.pp_print_string fmt (show_ttype t); ()

let compare t1 t2 = t1 = t2

let compare_type_lazy t t' = (match t', t with 
  | TList(_), TList(TAny) -> true 
  | TContract(_), TContract(TAny) -> true
  | a, b when a=b -> true
  | _, _ -> false
) 

let compare_list t1 t2 = 
  if List.length t1 = 1 && List.length t2 == 0 && List.hd t1 = TUnit then true 
  else List.length (List.filter (fun (a,b) -> a<>b) @@ List.combine t1 t2) = 0

