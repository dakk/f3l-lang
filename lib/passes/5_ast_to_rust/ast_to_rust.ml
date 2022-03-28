open Ast
open Format
open Helpers.Gen_utils
open Pp_lexpr
open Pp_ltype

let pp_types fmt types =
  let pp_def fmt (i, t) =
    fprintf fmt "type %s = @[%a@]@\n@\n" 
    i 
    pp_ltype t
  in
  (pp_list "@\n" pp_def) fmt @@ List.rev types


let pp_ast fmt ast = 
  let pp_s fmt (i, t) =
    match t with 
    | Ast.Def (t, e) -> 
      fprintf fmt "let %s = @[%a@]@\n" 
      i 
      pp_lexpr (t,e)
    | Ast.Type (t) ->
      fprintf fmt "type %s = @[%a@]@\n" 
      i 
      pp_ltype t
    | Ast.External (t, ie) ->
      fprintf fmt "external %s = @[%a@]@ %s\n" 
      i 
      pp_ltype t 
      ie

  in
  (pp_list "@\n" pp_s) fmt @@ ast


let generate_rust (ast: t) = 
  reset_temp ();
  pp_ast sfmt ast;
  sget ()