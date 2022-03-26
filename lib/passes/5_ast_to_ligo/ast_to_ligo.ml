open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils
open Pp_ltype
open Pp_lexpr




let generate_ligo_code (ast: t) = 
  reset_temp ();
  (* dump def *)
  pp_lexpr sfmt ast;
  sget ()


let generate_ligo (ast: t) = 
  generate_ligo_code ast