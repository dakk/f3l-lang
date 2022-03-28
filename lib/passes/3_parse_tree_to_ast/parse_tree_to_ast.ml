open Translate_pdecl

let translate (p: Parse_tree.t): Ast.t = 
  let e = transform p Env.start_env in 
  let rec itover sl = match sl with 
  | [] -> []
  | (i,st)::sl' -> (
    match st with 
    | Env.Type -> 
      let typ = List.assoc i (e.types) in
      itover sl' @ [(i,Ast.Type(typ))]
    | Env.Def -> 
      let dd = List.assoc i (e.defs) in
      itover sl' @ [(i,Ast.Def(dd))]
    | Env.External -> 
      let ex = List.assoc i (e.externals) in
      itover sl' @ [(i,Ast.External(ex))]
    | _ -> itover sl'
  )
  in itover e.symbols
  