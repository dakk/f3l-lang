open Ttype
open Expr

module Ast_ttype = Ttype
module Ast_expr = Expr
module Ast_expr_traversal = Expr_traversal

type te = Def of texpr | Type of ttype | External of (ttype * iden) [@@deriving show {with_path = false}]

type t = (iden * te) list [@@deriving show {with_path = false}]
