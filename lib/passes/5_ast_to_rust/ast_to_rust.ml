open Ast
open Format
open Helpers.Gen_utils
open Pp_lexpr
open Pp_ltype


let pp_ast fmt ast = 
  let pp_s fmt (i, t) =
    match t with 
    | Ast.Def (t, e) -> 
      fprintf fmt "let %s = @[%a@]@;\n" 
      i 
      pp_lexpr (t,e)
    | Ast.Type (t) when not (Ast_ttype.is_base t) ->
      fprintf fmt "type %s = @[%a@]@;\n" 
      i 
      pp_ltype t
    | Ast.External (t, ie) ->
      fprintf fmt "external %s = @[%a@]@ %s;\n" 
      i 
      pp_ltype t 
      ie
    | _ -> ()

  in
  (pp_list "" pp_s) fmt @@ ast


let generate_rust (ast: t) = 
  reset_temp ();
  (* fprintf sfmt "fn main() {\n"; *)
  pp_ast sfmt ast;
  (* fprintf sfmt "}\n"; *)
  sget ()