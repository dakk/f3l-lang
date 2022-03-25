open Ast
open Ast_ttype
open Translate_pexpr
open Translate_ptype
open Helpers.Errors
open Parsing
open Parse_tree

let rec transform (p: Parse_tree.t) (e: Env.t): Env.t = 
  match p with 
  (* type definition *)
  | Parse_tree.DType (dt) :: p' -> 
    Env.assert_symbol_absence e dt.id;

    transform p' { e with 
      symbols=(dt.id, Type)::e.symbols;
      types=(dt.id, transform_type dt.t e)::e.types;
    }

  (* global def *)
  | Parse_tree.DDef (dc) :: p' -> 
    Env.assert_symbol_absence e dc.id;

    let (t, exp) = (match dc.t with
    | None -> 
      let (t, exp) = transform_expr dc.v e [] in 
      (match (t) with
        | TList (TAny)
        | TOption (TAny) -> raise @@ TypeError(Pt_loc.dline p, "Unable to infer type of def '" ^ dc.id ^ "'")
        | _ -> (t, exp))
    | Some(ptt) ->
      let et = transform_type ptt e in
      let (t, exp) = transform_expr dc.v e [] in 

      let t = match (t, et) with
        | TList (TAny), TList (_) -> et
        | TOption (TAny), TOption (_) -> et
        | a, b when a = b -> t
        | _, _ -> raise @@ TypeError (Pt_loc.dline p, "Def '" ^ dc.id ^ "' expect to have type '" ^ show_ttype et ^ "', but type '" ^ show_ttype t ^ "' found")
      in t, exp)
    in transform p' { e with 
      symbols=(dc.id, Def)::e.symbols;
      defs=(dc.id, (t, exp))::e.defs;
    }


  | _ :: p' -> transform p' e
  | [] -> e