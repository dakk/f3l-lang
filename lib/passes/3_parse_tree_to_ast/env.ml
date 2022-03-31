open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors


type st = | Type | Union | Def | External [@@deriving show {with_path = false}]


type t = {
  types:      (iden * ttype) list;
  defs:       (iden * texpr) list;
  symbols:    (iden * st) list;
  externals:  (iden * (ttype * string)) list;
} [@@deriving show {with_path = false}]

let start_env = {
  defs=[];
  externals=[];
  types=[
    "'a", TAny;
    "unit", TUnit;
    "int", TInt;
    "float", TFloat;
    "bool", TBool;
    "string", TString;
    "char", TChar;
  ];
  symbols=[
    "'a", Type;
    "unit", Type;
    "int", Type;
    "float", Type;
    "bool", Type;
    "string", Type;
    "char", Type;
  ]
}

(* Fail if the symbol is already defined *)
let assert_symbol_absence (e: t) s = 
  match List.assoc_opt s e.symbols with 
  | None -> ()
  | Some (st) -> raise @@ DuplicateSymbolError (None, "Symbol '" ^ s ^ "' is already defined as " ^ show_st st)


let get_type_opt tn (e: t) = List.assoc_opt tn e.types

let get_ref sn (e: t) = 
  match List.assoc_opt sn e.symbols with 
  | None -> raise @@ SymbolNotFound(None, "Unknown reference to symbol '" ^ sn ^ "'")
  | Some (Def) -> let (tt, _) = List.assoc sn e.defs in tt  
  | Some (Union) -> let (tt, _) = List.assoc sn e.defs in tt  
  | Some (External) -> let rec rr l = (match l with 
    | (n,(tt,_))::l' -> if n = sn then tt else rr l' 
    | _ -> raise @@ SymbolNotFound(None, "Unknown reference to symbol '" ^ sn ^ "'")
  ) in rr e.externals
  | _ -> raise @@ SymbolNotFound(None, "Symbol '" ^ sn ^ "' not found in env")