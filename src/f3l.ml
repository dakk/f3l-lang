open Core
open F3l
open Helpers.Errors

let run action filename opt = 
  (match action with 
  | "compile" -> Compiler.compile filename opt
  | "extract-interface" -> Compiler.extract_interface filename opt
  | _ -> raise @@ CompilerError ("Invalid compiler action: " ^ action)
  )

let summary = ""
^ "=== actions ===\n\n"
^ "  compile file.f3l [-dcontract ContractName] [-target ligo|tz|coq]\n"
^ "                 compiles a contract ContractName to target language\n\n"
^ "  extract-interface file.f3l\n"
^ "                 extracts the f3l interface for the given module\n\n"


let command =
  Command.basic
    ~summary:"f3l-lang compiler"
    ~readme:(fun () -> summary)
    (let open Command.Let_syntax in
      let open Command.Param in
      let%map
            action      = anon ("action" %: string)
        and filename  = anon ("filename" %: string)
        and contract  = flag "-contract" (optional string) ~doc:" selected contract"
        and past      = flag "-print-ast" no_arg ~doc:" print ast"
        and ppt       = flag "-print-pt" no_arg ~doc:" print parse-tree"
        and pligo     = flag "-print-ligo" no_arg ~doc:" print ligo code"
        and verbose   = flag "-verbose" no_arg ~doc:" enable verbosity"
        and noremoveunused   = flag "-no-remove-unused" no_arg ~doc:" disable removing unused symbols"
        and target    = flag "-target" (optional string) ~doc:" target language (ligo, tz, coq)"
      in fun () -> 
        let opt = Compiler.{
          target = if is_none target then Some("tz") else target;
          contract = contract;
          print_pt = ppt;
          print_ast = past;
          print_ligo = pligo;
          verbose = verbose;
          no_remove_unused = noremoveunused;
        } in (
          let pp_err p cc m = pp_message p cc m true in 
          try run action filename opt with 
          | CompilerError (m) -> print_endline @@ pp_err None "CompilerError" m
          | SyntaxError (p,m) -> print_endline @@ pp_err p "SyntaxError" m
          | ParsingError (p,m) -> print_endline @@ pp_err p "ParsingError" m
          | TypeError (p,m) -> print_endline @@ pp_err p "TypeError" m
          | SymbolNotFound (p,m) -> print_endline @@ pp_err p "SymbolNotFound" m
          | DuplicateSymbolError (p,m) -> print_endline @@ pp_err p "DuplicateSymbolError" m
          | DeclarationError (p,m) -> print_endline @@ pp_err p "DeclarationError" m
          | InvalidExpression (p,m) -> print_endline @@ pp_err p "InvalidExpression" m
          | ContractError (p,m) -> print_endline @@ pp_err p "ContractError" m
          | APIError (p,m) -> print_endline @@ pp_err p "APIError" m
          | GenerateLigoError (p,m) -> print_endline @@ pp_err p "GenerateLigoError" m
        )
      )
        
let () = Command_unix.run ~version:"0.1" ~build_info:"git" command