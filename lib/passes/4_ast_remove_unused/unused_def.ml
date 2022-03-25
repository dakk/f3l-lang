open Ast
open Ast_expr
open Ast_expr_traversal
open Helpers

(* remove unused defs *)

module SymbolSet = Set.Make(String)

let used_globalref_in_expr (t, e) = 
  traverse (t, e) (fun (_, e) -> 
    match e with | GlobalRef (i) -> SymbolSet.singleton i
  ) SymbolSet.union SymbolSet.empty
  
let remove_unused _ (ast: Ast.t) = 
  let used = SymbolSet.union 
    (List.fold_left (fun acc (i,ce) -> SymbolSet.union acc (used_globalref_in_expr ce)) SymbolSet.empty ast.defs)
  in
  {
    ast with defs = List.filter (fun (i, _) -> 
      if SymbolSet.mem i used then true else (
        Errors.emit_warning None "Unused defant" @@ "The defant '" ^ i ^ "' is not used, dropping from ast";
        false)
    ) ast.defs
  }