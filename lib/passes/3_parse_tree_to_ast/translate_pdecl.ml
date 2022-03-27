open Ast
open Ast_ttype
open Ast_expr
open Translate_pexpr
open Translate_ptype
open Helpers.Errors
open Parsing
open Parse_tree

let rec transform (p: Parse_tree.t) (e: Env.t): Env.t = 
  match p with 
  (* type definition *)
  | Parse_tree.DType (id, t, poly) :: p' -> (
    Env.assert_symbol_absence e id;

    let tt = transform_type t e in 
    match tt with 
    | TUnion (el') -> 
      let rec consume el d s = match el with
      | [] -> d,s
      | ((x, xt) :: xs) ->
        Env.assert_symbol_absence e x;
        consume xs 
          ((x, (TUnion(el'), UnionValue(x, TUnion(el'), (xt, Unit))))::d) 
          ((x, Env.Union)::s)
      in 
      let (d,s) = consume el' [] [] in 
      transform p' { e with 
        symbols=(id, Type)::(s @ e.symbols);
        defs=d @ e.defs;
        types=(id, transform_type t e)::e.types;
      }
    | _ -> transform p' { e with 
      symbols=(id, Type)::e.symbols;
      types=(id, transform_type t e)::e.types;
    }
  )
  (* toplevel let *)
  | Parse_tree.DDef (id, tv, v) :: p' -> 
    Env.assert_symbol_absence e id;

    let (t, exp) = (match tv with
    | None -> 
      let (t, exp) = transform_expr v e [] in 
      (match (t) with
        | TList (TAny)
        | TOption (TAny) -> raise @@ TypeError(Pt_loc.dline p, "Unable to infer type of def '" ^ id ^ "'")
        | _ -> (t, exp))
    | Some(ptt) ->
      let et = transform_type ptt e in
      let (t, exp) = transform_expr v e [] in 

      let t = match (t, et) with
        | TList (TAny), TList (_) -> et
        | TOption (TAny), TOption (_) -> et
        | a, b when a = b -> t
        | _, _ -> raise @@ TypeError (Pt_loc.dline p, "Def '" ^ id ^ "' expect to have type '" ^ show_ttype et ^ "', but type '" ^ show_ttype t ^ "' found")
      in t, exp)
    in transform p' { e with 
      symbols=(id, Def)::e.symbols;
      defs=(id, (t, exp))::e.defs;
    }

  | Parse_tree.DExternal (id, t, n) :: p' ->  
    Env.assert_symbol_absence e id;
    let tt = transform_type t e in
    transform p' { e with 
      symbols=(id, External)::e.symbols;
      externals=(id, tt, n)::e.externals;
    }

  | [] -> e