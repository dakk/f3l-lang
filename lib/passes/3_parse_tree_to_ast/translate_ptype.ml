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
| Parse_tree.PTTuple (tl) -> 
  TTuple (List.map (fun tt -> transform_type tt e) tl)
| Parse_tree.PTCont (c, tt) -> (
  let assert_cmp_key a = if not (attributes a).cmp then 
    raise @@ TypeError (None, "Type '" ^ show_ttype a ^ "' is not comparable and cannot be used as key of " ^ c)
    else () 
  in
  let tt' = transform_type tt e in
  match c with 
  | "list" -> TList (tt') 
  | "option" -> TOption (tt')
  | c -> raise @@ TypeError (None, "Invalid container type '" ^ c ^ "'")
)
| Parse_tree.PTRecord (el) -> TRecord (List.map (fun (n, tt) -> n, transform_type tt e) el)
| Parse_tree.PTUnion (e) -> TUnion (e)
| Parse_tree.PTLambda (p, r) -> TLambda (transform_type p e, transform_type r e)

