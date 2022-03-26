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
      (* "list_methods", `Quick, compile opt None "test/type/list_methods.ml" None; *)
      "option_methods", `Quick, compile opt None "test/type/option_methods.ml" None;
    ];
    (* "expression", [
      "pack_unpack", `Quick, compile opt None "test/expr/pack_unpack.ml" None;
      "list_bfun", `Quick, compile opt None "test/expr/list_bfun.ml" None;
      "neg_fail", `Quick, compile opt (Some(APIError(None, ""))) "test/expr/neg_fail.ml" None;
      "record", `Quick, compile opt None "test/expr/record.ml" None;
      "record_nest", `Quick, compile opt None "test/expr/record_nest.ml" None;
      "literal", `Quick, compile opt None "test/expr/literal.ml" None;
      "literal_untyped", `Quick, compile opt None "test/expr/literal_untyped.ml" None;
      "lambda", `Quick, compile opt None "test/expr/lambda.ml" None;
      "assoc_bool", `Quick, compile opt None "test/expr/assoc_bool.ml" None;
      "match_case", `Quick, compile opt None "test/expr/match_case.ml" None;
      "tuple_destruct_typed", `Quick, compile opt None "test/expr/tuple_destruct_typed.ml" None;
      "tuple_destruct_untyped", `Quick, compile opt None "test/expr/tuple_destruct_untyped.ml" None;
    ];
    "define", [
      "def", `Quick, compile opt None "test/def/def.ml" None;
      "numeric", `Quick, compile opt None "test/def/numeric.ml" None;
      "string", `Quick, compile opt None "test/def/string.ml" None;
      "lambda", `Quick, compile opt None "test/def/lambda.ml" None;
      "lambda_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/def/lambda_fail.ml" None;
      "expr", `Quick, compile opt None "test/def/expr.ml" None;
      "let_expr", `Quick, compile opt None "test/def/let_expr.ml" None;
      "enum", `Quick, compile opt None "test/def/enum.ml" None;
      "infer", `Quick, compile opt None "test/def/infer.ml" None;
      "let_infer", `Quick, compile opt None "test/def/let_infer.ml" None;
    ]; *)
    "module", [
      "t1", `Quick, compile opt None "test/module/t1.ml" None;
      "open", `Quick, compile opt None "test/module/open.ml" None;
    ];
  ]
