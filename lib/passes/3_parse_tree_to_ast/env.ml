open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors


type st = | Type | Union | Def | External [@@deriving show {with_path = false}]


type t = {
  types:      (iden * ttype) list;
  defs:       (iden * texpr) list;
  symbols:    (iden * st) list;
  externals:  (iden * ttype * string) list;
} [@@deriving show {with_path = false}]

let start_env = {
  defs=[];
  externals=[];
  types=[
    "unit", TUnit;
    "int", TInt;
    "nat", TNat;
    "float", TFloat;
    "bool", TBool;
    "string", TString;
    "bytes", TBytes;
  ];
  symbols=[
    "unit", Type;
    "int", Type;
    "nat", Type;
    "float", Type;
    "bool", Type;
    "string", Type;
    "bytes", Type;
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
  | _ -> raise @@ SymbolNotFound(None, "Symbol '" ^ sn ^ "' not found in env")