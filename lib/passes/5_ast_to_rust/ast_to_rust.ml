open Ast
open Format
open Helpers.Gen_utils
open Pp_lexpr


let pp_defs fmt defs = 
  let pp_def fmt (i, (t, e)) =
    fprintf fmt "let %s = @[%a@]@\n@\n" 
    i 
    pp_lexpr (t,e)
  in
  (pp_list "@\n" pp_def) fmt @@ List.rev defs


let generate_rust (ast: t) = 
  reset_temp ();
  (* dump def *)
  pp_defs sfmt ast.defs;
  sget ()