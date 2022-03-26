open Parse_tree
open Helpers.Errors

(* find file m in paths ip *)
let rec find_file (ip: string list) (m: string) = 
  match ip with
  | [] -> raise (ModuleNotFound m)
  | (d::ip') -> 
    let f = d ^ "/" ^ (String.uncapitalize_ascii m) ^ ".ml" in
    if Sys.file_exists f then f else find_file ip' m
;;


(* replace all opens in a Parse_tree with the content of the file *)
let rec inject_opens (ip: string list) (pt: Parse_tree.t): Parse_tree.t =
  let inject m = 
    let f = find_file ip m in
    try Parsing.parse_file f |> inject_opens ip
    with | e -> raise e
  in
  match pt with 
  | Parse_tree.PESeq (Parse_tree.PEOpen (m), b) -> 
    Parse_tree.PESeq (inject m, inject_opens ip b)
  | Parse_tree.PESeq (b, Parse_tree.PEOpen (m)) -> 
    Parse_tree.PESeq (inject_opens ip b, inject m) 
  | Parse_tree.PESeq (Parse_tree.PEOpen (m), Parse_tree.PEOpen (m')) -> 
    Parse_tree.PESeq (inject m, inject m')
  | p -> p