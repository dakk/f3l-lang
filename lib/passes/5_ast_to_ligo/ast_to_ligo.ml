open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils
open Pp_ltype
open Pp_lexpr


let pp_defs fmt defs = 
  let pp_def fmt (i, (t, e)) =
    fprintf fmt "let %s = @[%a@]@\n@\n" 
    i 
    pp_lexpr (t,e)
  in
  (pp_list "@\n" pp_def) fmt @@ List.rev defs


 



let generate_ligo_code (ast: t) = 
  reset_temp ();
  (* dump def *)
  pp_defs sfmt ast.defs;
  sget ()


let generate_ligo (ast: t) = 
  generate_ligo_code ast