open Ast 

let rec remove_base_types a = match a with 
| [] -> []
| ((_, Type (x)) :: xs) when Ast_ttype.is_base x -> remove_base_types xs
| (x :: xs) -> x :: remove_base_types xs
