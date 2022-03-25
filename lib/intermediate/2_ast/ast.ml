open Ttype
open Expr

module Ast_ttype = Ttype
module Ast_expr = Expr
module Ast_expr_traversal = Expr_traversal

type ctor = {
  arg: (iden * ttype) list;
  exprs: (iden * texpr) list;
} [@@deriving show {with_path = false}]


type contract = {
  fields: (iden * ttype) list;
  entries: entry list;
} [@@deriving show {with_path = false}]

type t = {
  consts:      (iden * texpr) list;
  contracts:   (iden * contract) list;
} [@@deriving show {with_path = false}]
