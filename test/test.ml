open F3l
open Helpers.Errors

let opt = Compiler.{
  target = None;
  print_pt = true;
  print_ast = true;
  verbose = false;
  no_remove_unused = true;
  include_paths = ["."; "./test/module"; "./test/expr"];
}

let optc = { opt with no_remove_unused = false }

let optl = { opt with target=Some("rust") }
let optlc = { optc with target=Some("rust") }


let compile opt exc path cname _ = 
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
      "comment", `Quick, compile opt None "test/compiler/comment.ml" None;
      "comment_multiline", `Quick, compile opt None "test/compiler/comment_multiline.ml" None;
      "external_def", `Quick, compile opt None "test/compiler/external_def.ml" None;
      "external_fun", `Quick, compile opt None "test/compiler/external_fun.ml" None;
    ];
    "type", [
      "types", `Quick, compile opt None "test/type/types.ml" None;
    ];
    "expression", [
      (* "pack_unpack", `Quick, compile opt None "test/expr/pack_unpack.ml" None; *)
      "record", `Quick, compile opt None "test/expr/record.ml" None;
      "literal", `Quick, compile opt None "test/expr/literal.ml" None;
      "literal_untyped", `Quick, compile opt None "test/expr/literal_untyped.ml" None;
      "lambda", `Quick, compile opt None "test/expr/lambda.ml" None;
      "assoc_bool", `Quick, compile opt None "test/expr/assoc_bool.ml" None;
      "pair_fstsnd", `Quick, compile opt None "test/expr/pair_fstsnd.ml" None;
      "pair_opt", `Quick, compile opt None "test/expr/pair_opt.ml" None;
      "rec", `Quick, compile opt None "test/expr/rec.ml" None;
      "pair_lst", `Quick, compile opt None "test/expr/pair_lst.ml" None;
      "pair_fstsnd_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/expr/pair_fstsnd_fail.ml" None;
      (* "match_case", `Quick, compile opt None "test/expr/match_case.ml" None; *)
      (* "pair_destruct_typed", `Quick, compile opt None "test/expr/pair_destruct_typed.ml" None;
      "pair_destruct_untyped", `Quick, compile opt None "test/expr/pair_destruct_untyped.ml" None; *)
    ];
    "define", [
      "def", `Quick, compile opt None "test/def/def.ml" None;
      "numeric", `Quick, compile opt None "test/def/numeric.ml" None;
      "string", `Quick, compile opt None "test/def/string.ml" None;
      "lambda", `Quick, compile opt None "test/def/lambda.ml" None;
      "lambda_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/lambda_fail.ml" None;
      "expr", `Quick, compile opt None "test/def/expr.ml" None;
      "let_expr", `Quick, compile opt None "test/def/let_expr.ml" None;
      "union", `Quick, compile opt None "test/def/union.ml" None;
      (* "union_typed", `Quick, compile opt None "test/def/union_typed.ml" None; *)
      (* "union_opt_typedef", `Quick, compile opt None "test/def/union_opt_typedef.ml" None; *)
      (* "union_typed_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/union_typed_fail.ml" None;
      "union_typed_fail2", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/union_typed_fail2.ml" None; *)
      "union_dup_fail", `Quick, compile opt (Some(DuplicateSymbolError(None, ""))) "test/def/union_dup_fail.ml" None;
      "infer", `Quick, compile opt None "test/def/infer.ml" None;
      "let_infer", `Quick, compile opt None "test/def/let_infer.ml" None;
    ];
    "module", [
      "open", `Quick, compile opt None "test/module/open.ml" None;
      (* "module", `Quick, compile opt None "test/module/module.ml" None; *)
    ];
  ]
