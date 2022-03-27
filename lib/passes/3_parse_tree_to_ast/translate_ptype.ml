open Ast
open Ast_ttype
open Helpers.Errors
open Parsing

(* transform a pttype to a ttype *)
let rec transform_type (pt: Parse_tree.ptype) (e: Env.t): ttype = match pt with 
| Parse_tree.PTBuiltin (tn) -> 
  (match Env.get_type_opt tn e with 
  | None -> raise @@ TypeError (None, "Undefined type '" ^ tn ^ "'")
  | Some (t) -> t)
| Parse_tree.PTPair (tl1, tl2) -> 
  TPair (transform_type tl1 e, transform_type tl2 e)
| Parse_tree.PTCont (c, tt) -> (
  let assert_cmp_key a = if not (attributes a).cmp then 
    raise @@ TypeError (None, "Type '" ^ show_ttype a ^ "' is not comparable and cannot be used as key of " ^ c)
    else () 
  in
  let tt' = transform_type tt e in
  match c with 
  | c -> raise @@ TypeError (None, "Invalid container type '" ^ c ^ "'")
)
| Parse_tree.PTRecord (el) -> TRecord (List.map (fun (n, tt) -> n, transform_type tt e) el)
| Parse_tree.PTUnion (tl) -> TUnion (tl)
| Parse_tree.PTLambda (p, r) -> TLambda (transform_type p e, transform_type r e)

