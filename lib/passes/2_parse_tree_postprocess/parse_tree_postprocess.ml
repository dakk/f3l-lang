open Parse_tree
open Helpers.Errors

(* find file m in paths ip *)
let rec find_file (ip: string list) (m: string) = 
  match ip with
  | [] -> raise (ImportedModuleNotFound m)
  | (d::ip') -> 
    let f = d ^ "/" ^ (String.uncapitalize_ascii m) ^ ".ml" in
    if Sys.file_exists f then f else find_file ip' m
;;


(* replace all includes in a Parse_tree with the content of the file *)
let rec inject_include (ip: string list) (pt: Parse_tree.t): Parse_tree.t =
  List.fold_left (fun ptl dec -> 
    match dec with 
    | Parse_tree.DInclude (m) -> (
      let f = find_file ip m in
      try ptl @ Parsing.parse_file f |> inject_include ip
      with | e -> raise e)
    | _ -> ptl @ [dec]
  ) [] pt

