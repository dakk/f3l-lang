open Printf
open Helpers.Errors

type options = {
  target: string option;
  print_pt: bool;
  print_ast: bool;
  verbose: bool;
  no_remove_unused: bool;
  include_paths: string list;
}

let default_options = {
  target = Some ("c");
  print_pt = true;
  print_ast = true;
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

let print_str s _ = s |> print_endline; print_endline ""

(* [ap b f] conditionally apply f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

(* [app b f] conditionally apply f or iden if b or not b, return the same value *)
let app b f = if b then (fun x -> let _: unit = f x in x) else (fun x -> x)


(* text_file => ast *)
let build_ast (filename: string) opt =
  if opt.verbose then printf "===> Parsing %s\n\n%!" filename;

  (* parse the starting file *)
  let pt = filename |> Passes.Parsing.parse_file in

  (* parse and inject opens *)
  pt
  |> app opt.verbose @@ print_str "===> Injecting opens";
  |> Passes.Parse_tree_postprocess.inject_opens opt.include_paths


  (* parse and inject opens *)

  (* print pt *)
  |> app opt.print_pt print_pt

  (* transform pt to ast *)
  |> app opt.verbose @@ print_str "===> Translating Parse_tree to Ast"
  |> Passes.Parse_tree_to_ast.translate 

  (* print ast *)
  |> app opt.print_ast print_ast 

    
let write_file (filename: string) data = 
  let oc = open_out filename in 
  fprintf oc "%s" data;
  close_out oc 

(* text_file => ast => target *)
let compile (filename: string) opt =
  build_ast filename opt
    (* remove unused *)
    |> app opt.verbose @@ print_str "===> Dropping unused code" 
    (* |> ap (not opt.no_remove_unused) @@ Passes.Ast_remove_unused.remove_unused opt.contract *)
    |> app opt.print_ast print_ast 

    (* output to a final language - first pass *)
    |> (fun ast -> 
      match opt.target with 
      | None -> ""
      | Some ("ligo") -> 
        if opt.verbose then printf "===> Generating ligo code\n\n%!";        
        Passes.Ast_to_ligo.generate_ligo ast
    )
    |> print_endline

