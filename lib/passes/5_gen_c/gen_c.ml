open Ast
open Format
open Helpers.Gen_utils
open Pp_lexpr
open Pp_ltype


let pp_ast fmt ast = 
  let pp_s fmt (i, t) =
    match t with 
    | Ast.Def (t, e) -> 
      fprintf fmt "auto %s = []() { @[%a@]@ }()\n" 
      (* pp_ltype t  *)
      i 
      pp_lexpr (t,e)
    | Ast.External (_, ie) ->
      fprintf fmt "#define %s = %s\n" 
      i 
      ie
    | _ -> ()

  in
  (pp_list "" pp_s) fmt @@ ast


let generate_c (ast: t) = 
  reset_temp ();

  (* fprintf sfmt "#define lambda(return_type, function_body) \
    ({ return_type __fn__ function_body __fn__; })\n"; *)

  (* fprintf sfmt "struct pair {
    void *fst;
    void *snd;
  };\n"; *)
  
  pp_ast sfmt ast;
  
  sget ()