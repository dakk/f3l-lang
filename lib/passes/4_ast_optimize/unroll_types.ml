(* open Ast  *)

(* remove all typedef unrolling types on every expression *)
(* let unroll_types ast = 
  let unroll_exp e tass = 
    Ast_expr_traversal.traverse e (fun e -> 
      e
    )
  in 
  let rec uroll a tass = match a with 
  | [] -> []
  | (i, Type (t)) :: xs -> uroll xs (tass @ [(i,t)])
  | (i, Def (t, e)) :: xs -> (i,Def (t |> Ast_ttype.type_final, e |> unroll_exp))::(uroll xs tass)
  | a :: xs -> a::(uroll xs tass)
  in uroll ast [] *)
