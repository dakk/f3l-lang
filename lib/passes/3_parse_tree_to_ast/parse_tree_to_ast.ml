open Translate_pexpr

let translate (p: Parse_tree.t): Ast.t = 
  transform_expr p Env.start_env []
  