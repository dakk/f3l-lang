open F3l
open Helpers.Errors

let opt = Compiler.{
  target = None;
  print_pt = true;
  print_uast = true;
  print_ast = true;
  verbose = false;
  no_remove_unused = true;
  include_paths = ["."; "./test/module"; "./test/expr"];
}

let optc = { opt with no_remove_unused = false }

let optl = { opt with target=Some("c") }
let optlc = { optc with target=Some("c") }


let compile opt exc path _ = 
  let compile_failure = try (Compiler.compile path opt; None) with 
  | f -> Some(f) in
  (match compile_failure, exc with 
  | None, None -> ()
  | Some(SyntaxError(_,_)), Some(SyntaxError(_,_)) -> ()
  | Some(APIError(_,_)), Some(APIError(_,_)) -> ()
  | Some(TypeError(_,_)), Some(TypeError(_,_)) -> ()
  | Some(DuplicateSymbolError(_,_)), Some(DuplicateSymbolError(_,_)) -> ()
  | Some(DeclarationError(_,_)), Some(DeclarationError(_,_)) -> ()
  | Some(SymbolNotFound(_,_)), Some(SymbolNotFound(_,_)) -> ()
  | Some(f), _ -> failwith @@ "Invalid" ^ Printexc.to_string f
  | None, Some(e) -> failwith @@ "Expected an exception, none catched: " ^ Printexc.to_string e)




let () =
  Alcotest.run "yallo" [
    "compiler", [
      "comment", `Quick, compile opt None "test/compiler/comment.ml";
      "comment_multiline", `Quick, compile opt None "test/compiler/comment_multiline.ml";
      "external_def", `Quick, compile opt None "test/compiler/external_def.ml";
      "external_fun", `Quick, compile opt None "test/compiler/external_fun.ml";
    ];
    "type", [
      "types", `Quick, compile opt None "test/type/types.ml";
      "atype", `Quick, compile opt None "test/type/atype.ml";
      (* "atype_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/type/atype_fail.ml"; *)
    ];
    "sugar", [
      "match_case", `Quick, compile opt None "test/sugar/match_case.ml";
      "pipegt", `Quick, compile opt None "test/sugar/pipegt.ml";
      "currying", `Quick, compile opt None "test/sugar/currying.ml";
      "n_uple", `Quick, compile opt None "test/sugar/n_uple.ml";
      "list", `Quick, compile opt None "test/sugar/list.ml";
      "typed_union", `Quick, compile opt None "test/sugar/typed_union.ml";
    ];
    "expression", [
      "literal", `Quick, compile opt None "test/expr/literal.ml";
      "literal_untyped", `Quick, compile opt None "test/expr/literal_untyped.ml";
      "lambda", `Quick, compile opt None "test/expr/lambda.ml";
      "assoc_bool", `Quick, compile opt None "test/expr/assoc_bool.ml";
      "pair_fstsnd", `Quick, compile opt None "test/expr/pair_fstsnd.ml";
      "pair_opt", `Quick, compile opt None "test/expr/pair_opt.ml";
      "rec", `Quick, compile opt None "test/expr/rec.ml";
      "pair_lst", `Quick, compile opt None "test/expr/pair_lst.ml";
      "pair_fstsnd_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/expr/pair_fstsnd_fail.ml";
      (* "pair_destruct_typed", `Quick, compile opt None "test/expr/pair_destruct_typed.ml";
      "pair_destruct_untyped", `Quick, compile opt None "test/expr/pair_destruct_untyped.ml"; *)
    ];
    "define", [
      "def", `Quick, compile opt None "test/def/def.ml";
      "numeric", `Quick, compile opt None "test/def/numeric.ml";
      "string", `Quick, compile opt None "test/def/string.ml";
      "lambda", `Quick, compile opt None "test/def/lambda.ml";
      "lambda_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/lambda_fail.ml";
      "expr", `Quick, compile opt None "test/def/expr.ml";
      "let_expr", `Quick, compile opt None "test/def/let_expr.ml";
      "union", `Quick, compile opt None "test/def/union.ml";
      (* "union_typed", `Quick, compile opt None "test/def/union_typed.ml"; *)
      (* "union_opt_typedef", `Quick, compile opt None "test/def/union_opt_typedef.ml"; *)
      (* "union_typed_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/union_typed_fail.ml";
      "union_typed_fail2", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/union_typed_fail2.ml"; *)
      "union_dup_fail", `Quick, compile opt (Some(DuplicateSymbolError(None, ""))) "test/def/union_dup_fail.ml";
      "infer", `Quick, compile opt None "test/def/infer.ml";
      "let_infer", `Quick, compile opt None "test/def/let_infer.ml";
    ];
    "module", [
      "open", `Quick, compile opt None "test/module/open.ml";
      (* "module", `Quick, compile opt None "test/module/module.ml"; *)
    ];
  ]
