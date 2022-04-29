open Ast
open Format
open Helpers.Gen_utils
open Pp_lexpr
open Pp_ltype


let pp_ast fmt ast = 
  let rec pp_s fmt (i, t) =
    match t with 

    | Ast.Def (TLambda(_,_), Lambda((ia,ta), (t, e))) -> 
      fprintf fmt "auto %s = [](auto %s) { return %a; };\n" 
      i 
      (* pp_ltype ta *)
      ia 
      pp_lexpr (t,e)

    | Ast.Def (t, LetIn(il, tl, el, e2)) -> 
      fprintf fmt "%a %s = %a;\n%a;\n" 
      pp_ltype t
      il
      pp_lexpr el
      pp_s (i, Ast.Def(fst e2, snd e2))

    | Ast.Def (t, e) -> 
      fprintf fmt "%s := %a;\n" 
      i 
      pp_ltype t
      pp_lexpr (t,e)
    | Ast.External (_, ie) ->
      fprintf fmt "#define %s %s\n" 
      i 
      ie
    | _ -> ()

  in
  (pp_list "" pp_s) fmt @@ ast


let generate_v (ast: t) = 
  reset_temp ();

  (* fprintf sfmt "#define lambda(return_type, function_body) \
    ({ return_type __fn__ function_body __fn__; })\n"; *)

  (* fprintf sfmt "struct pair {
    void *fst;
    void *snd;
  };\n"; *)
  
  pp_ast sfmt ast;

  
  pdefs_tostring () ^ "\n\n" ^ sget ()