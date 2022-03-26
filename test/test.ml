open F3l
open Helpers.Errors

let opt = Compiler.{
  target = None;
  print_pt = false;
  print_ast = false;
  verbose = false;
  no_remove_unused = true;
  include_paths = ["."; "./test/module"];
}

let optc = { opt with no_remove_unused = false }

let optl = { opt with target=Some("ligo") }
let optlc = { optc with target=Some("ligo") }


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
      "comments", `Quick, compile opt None "test/compiler/comments.ml" None;
      "external", `Quick, compile opt None "test/compiler/external.ml" None;
    ];
    "type", [
      "types", `Quick, compile opt None "test/type/types.ml" None;
      (* "list_methods", `Quick, compile opt None "test/type/list_methods.yallo" None; *)
      "option_methods", `Quick, compile opt None "test/type/option_methods.yallo" None;
    ];
    "expression", [
      "pack_unpack", `Quick, compile opt None "test/expr/pack_unpack.yallo" None;
      "list_bfun", `Quick, compile opt None "test/expr/list_bfun.yallo" None;
      "neg_fail", `Quick, compile opt (Some(APIError(None, ""))) "test/expr/neg_fail.yallo" None;
      "record", `Quick, compile opt None "test/expr/record.yallo" None;
      "literal", `Quick, compile opt None "test/expr/literal.yallo" None;
      "literal_untyped", `Quick, compile opt None "test/expr/literal_untyped.yallo" None;
      (* "literal_fail_infer", `Quick, compile opt (Some(TypeError(None, ""))) "test/expr/literal_fail_infer.yallo" None; *)
      "lambda", `Quick, compile opt None "test/expr/lambda.yallo" None;
      "assoc_bool", `Quick, compile opt None "test/expr/assoc_bool.yallo" None;
      "match_case", `Quick, compile opt None "test/expr/match_case.yallo" None;
      "tuple_destruct_typed", `Quick, compile opt None "test/expr/tuple_destruct_typed.yallo" None;
      "tuple_destruct_untyped", `Quick, compile opt None "test/expr/tuple_destruct_untyped.yallo" None;
    ];
    "define", [
      "def", `Quick, compile opt None "test/def/def.yallo" None;
      "numeric", `Quick, compile opt None "test/def/numeric.yallo" None;
      "string", `Quick, compile opt None "test/def/string.yallo" None;
      "lambda", `Quick, compile opt None "test/def/lambda.yallo" None;
      "lambda_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/lambda_fail.yallo" None;
      "expr", `Quick, compile opt None "test/def/expr.yallo" None;
      "let_expr", `Quick, compile opt None "test/def/let_expr.yallo" None;
      "enum", `Quick, compile opt None "test/def/enum.yallo" None;
      "infer", `Quick, compile opt None "test/def/infer.yallo" None;
      "let_infer", `Quick, compile opt None "test/def/let_infer.yallo" None;
    ];
    "module", [
      "t1", `Quick, compile opt None "test/module/t1.ml" None;
      "open_type", `Quick, compile opt None "test/module/open_type.ml" None;
    ];
  ]
