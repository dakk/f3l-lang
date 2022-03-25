open Ttype
open Expr

module Ast_ttype = Ttype
module Ast_expr = Expr
module Ast_expr_traversal = Expr_traversal


type t = {
  defs:      (iden * texpr) list;
} [@@deriving show {with_path = false}]
