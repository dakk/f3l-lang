open Untyped_ast
open Format
open Helpers.Gen_utils
open Pp_lexpr


let pp_uast fmt uast = 
  let pp_s fmt (i, t) =
    match t with 
    | Untyped_ast.UDef (e) -> 
      fprintf fmt "%s = @[%a@]@\n" 
      i 
      pp_lexpr (e)
    | Untyped_ast.UExternal (ie) ->
      fprintf fmt "TODOEXTERNAL %s = %s\n" 
      i 
      ie
    | _ -> ()

  in
  (pp_list "" pp_s) fmt @@ uast


let generate_py (uast: t) = 
  reset_temp ();
  
  pp_uast sfmt uast;
  
  sget ()