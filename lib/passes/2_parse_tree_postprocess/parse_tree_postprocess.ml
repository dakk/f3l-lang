open Helpers.Errors

(* find file m in paths ip *)
let rec find_file (ip: string list) (m: string) = 
  match ip with
  | [] -> raise (ModuleNotFound m)
  | d::ip' -> 
    let f = d ^ "/" ^ (String.uncapitalize_ascii m) ^ ".ml" in
    Printf.printf "checking in %s for module %s\n" f m;
    if Sys.file_exists f then (Printf.printf "%s found\n" f; f) else find_file ip' m
;;


(* replace all opens in a Parse_tree with the content of the file *)
let rec inject_opens (ip: string list) (pt: Parse_tree.t): Parse_tree.t =
  Printf.printf "%s\n" (List.fold_left (^) "" ip);
  List.fold_left (fun ptl dec -> 
    match dec with 
    | Parse_tree.DOpen (m) -> (
      let f = find_file ip m in
      try ptl @ Parsing.parse_file f |> inject_opens ip
      with | e -> raise e)
    | _ -> ptl @ [dec]
  ) [] pt

