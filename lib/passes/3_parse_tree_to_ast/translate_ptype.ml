open Ast
open Ast_ttype
open Helpers.Errors

(* transform a pttype to a ttype *)
let rec transform_type (pt: Parse_tree.ptype) (e: Env.t): ttype = match pt with 
| Parse_tree.PTBuiltin (tn) -> 
  (match Env.get_type_opt tn e with 
  | None -> raise @@ TypeError (None, "Undefined type '" ^ tn ^ "'")
  | Some (t) -> t)
| Parse_tree.PTPair (tl1, tl2) -> 
  TPair (transform_type tl1 e, transform_type tl2 e)
| Parse_tree.PTRecord (el) -> TRecord (List.map (fun (n, tt) -> n, transform_type tt e) el)
| Parse_tree.PTUnion (tl) -> TUnion (tl)
| Parse_tree.PTLambda (p, r) -> TLambda (transform_type p e, transform_type r e)

