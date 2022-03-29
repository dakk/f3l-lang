open Printf

type options = {
  target: string option;
  print_pt: bool;
  print_ast: bool;
  print_uast: bool;
  verbose: bool;
  no_remove_unused: bool;
  include_paths: string list;
}

let default_options = {
  target = Some ("rust");
  print_pt = true;
  print_ast = true;
  print_uast = true;
  verbose = true;
  no_remove_unused = false;
  include_paths = ["."];
}

(* dump the parse tree, debug only *)
let print_pt (pt: Intermediate.Parse_tree.t) = 
  pt 
  |> Intermediate.Parse_tree.show 
  |> print_endline; print_endline ""

(* dump the ast, debug only *)
let print_ast (ast: Intermediate.Ast.t) = 
  ast 
  |> Intermediate.Ast.show 
  |> print_endline; print_endline ""

(* dump the uast, debug only *)
let print_uast (ast: Intermediate.Untyped_ast.t) = 
  ast 
  |> Intermediate.Untyped_ast.show 
  |> print_endline; print_endline ""

let print_str s _ = s |> print_endline; print_endline ""

(* [ap b f] conditionally apply f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

(* [app b f] conditionally apply f or iden if b or not b, return the same value *)
let app b f = if b then (fun x -> let _: unit = f x in x) else (fun x -> x)


(* text_file => pt *)
let build_pt (filename: string) opt =
  if opt.verbose then printf "===> Parsing %s\n\n%!" filename;

  (* parse the starting file *)
  filename 
    |> Passes.Parsing.parse_file
    |> app opt.verbose @@ print_str "===> Injecting opens"
    |> Passes.Parse_tree_postprocess.inject_opens opt.include_paths
    (* print pt *)
    |> app opt.print_pt print_pt
  

(* pt => ast *)
let build_ast (pt: Parse_tree.t) opt =
  pt
    (* transform pt to ast *)
    |> app opt.verbose @@ print_str "===> Translating Parse_tree to Ast"
    |> Passes.Parse_tree_to_ast.translate 

    (* replace unions *)
    |> app opt.verbose @@ print_str "===> Replacing unions" 
    |> Passes.Ast_optimize_1_replace_union_with_nat.replace_union_with_nat 

    (* remove unused *)
    |> app opt.verbose @@ print_str "===> Dropping unused code" 
    |> ap (not opt.no_remove_unused) @@ Passes.Ast_optimize_2_remove_unused.remove_unused 

    (* print ast *)
    |> app opt.print_ast print_ast 


(* ast => uast *)
let build_uast ast opt = 
  ast 
    (* transform ast to uast *)
    |> app opt.verbose @@ print_str "===> Translating Ast to Uast"
    |> Passes.Ast_to_uast.translate 

    (* print uast *)
    |> app opt.print_uast print_uast
    
let write_file (filename: string) data = 
  let oc = open_out filename in 
  fprintf oc "%s" data;
  close_out oc 

(* text_file => ast => target *)
let compile (filename: string) opt =
  let pt = build_pt filename opt in
  let ast = build_ast pt opt in
  let uast = build_uast ast opt in ()
    (* |> (fun ast -> 
      match opt.target with 
      | Some ("rust") -> 
        if opt.verbose then printf "===> Generating rust code\n\n%!";        
        Passes.Ast_to_rust.generate_rust ast
      | _ -> ""
    )
    |> print_endline *)

