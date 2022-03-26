open Ast 
open Ast_expr 
open Ast_ttype
open Lexing
open Printf

module AstEmp = struct 
  type t = Expr of texpr
end 

module LocationTable = Helpers.Loc.LocationTable (AstEmp)

type l = Helpers.Loc.l

let filename = ref ""
let locations = ref @@ LocationTable.create 10000
let ladd = LocationTable.add !locations
let pp_pos s = s, !filename, s.pos_lnum, (s.pos_cnum - s.pos_bol + 1)

let loce s x = ladd (Expr (x)) @@ pp_pos s; x

let eline ee: l option = try Some (LocationTable.find !locations @@ Expr (ee)) with | _ -> None