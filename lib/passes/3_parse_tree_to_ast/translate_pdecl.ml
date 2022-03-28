open Ast
open Ast_ttype
open Ast_expr
open Translate_pexpr
open Translate_ptype
open Helpers.Errors
open Parsing

let rec transform (p: Parse_tree.t) (e: Env.t): Env.t = 
  match p with 
  (* type definition *)
  | Parse_tree.DType (id, t) :: p' -> (
    Env.assert_symbol_absence e id;

    let temp_env = { e with 
      symbols=(id, Type)::e.symbols;
      types=(id, TAny)::e.types;
    } in
    let tt = transform_type t temp_env in 

    match tt with 
    | TUnion (el') -> 
      let rec consume el d s = match el with
      | [] -> d,s
      | x :: xs ->
        Env.assert_symbol_absence e x;
        consume xs 
          ((x, (TUnion(el'), UnionValue(x)))::d) 
          ((x, Env.Union)::s)
      in 
      let (d,s) = consume el' [] [] in 
      transform p' { e with 
        symbols=(id, Type)::(s @ e.symbols);
        defs=d @ e.defs;
        types=(id, tt)::e.types;
      }
    | _ -> transform p' { e with 
      symbols=(id, Type)::e.symbols;
      types=(id, tt)::e.types;
    }
  )
  (* toplevel let *)
  | Parse_tree.DDef (id, tv, v, r) :: p' -> 
    Env.assert_symbol_absence e id;

    let temp_env = if r then { e with 
      symbols=(id, Def)::e.symbols;
      defs=(id, (TLambda(TAny, TAny), Unit))::e.defs;
    } else e in

    let (t, exp) = (match tv with
    | None -> 
      let (t, exp) = transform_expr v temp_env [] in 
      (match (t) with
        | _ -> (t, exp))
    | Some(ptt) ->
      let et = transform_type ptt temp_env in
      let (t, exp) = transform_expr v temp_env [] in 

      let t = match (t, et) with
        | a, b when a = b -> t
        | _, _ -> raise @@ TypeError (Pt_loc.dline p, "Def '" ^ id ^ "' expect to have type '" ^ show_ttype et ^ "', but type '" ^ show_ttype t ^ "' found")
      in t, exp)
    in transform p' { e with 
      symbols=(id, Def)::e.symbols;
      defs=(id, (t, exp))::e.defs;
    }

  (* external function *)
  | Parse_tree.DExternal (id, t, n) :: p' ->  
    Env.assert_symbol_absence e id;
    let tt = transform_type t e in
    transform p' { e with 
      symbols=(id, External)::e.symbols;
      externals=(id, (tt, n))::e.externals;
    }

  | Parse_tree.DOpen (_) :: p' -> 
    transform p' e

  | [] -> e