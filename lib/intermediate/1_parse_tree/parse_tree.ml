type iden = string [@@deriving show {with_path = false}]

type ptype = 
  | PTBuiltin of string               (* type name *)
  | PTPair of ptype * ptype           (* pair of other types *)
  | PTUnion of string list
  | PTLambda of ptype * ptype
  [@@deriving show {with_path = false}]

type pexpr =
  | PEUnit
  | PEBool of bool
  | PEInt of int 
  | PEFloat of float
  | PEString of string
  | PEBytes of string
  | PETyped of pexpr * ptype
  | PEPair of pexpr * pexpr
  | PELambda of (iden * ptype) * pexpr

  | PERef of iden
  | PEIfThenElse of pexpr * pexpr * pexpr 
  | PEApply of pexpr * pexpr
  | PELetIn of iden * ptype option * pexpr * pexpr * bool

  (* aritmetic *)
  | PEAdd of pexpr * pexpr
  | PESub of pexpr * pexpr
  | PEMul of pexpr * pexpr
  | PEDiv of pexpr * pexpr
  | PEMod of pexpr * pexpr
  | PEFAdd of pexpr * pexpr
  | PEFSub of pexpr * pexpr
  | PEFMul of pexpr * pexpr
  | PEFDiv of pexpr * pexpr

  (* bool *)
  | PEAnd of pexpr * pexpr
  | PEOr of pexpr * pexpr
  | PENot of pexpr
  | PELt of pexpr * pexpr
  | PELte of pexpr * pexpr
  | PEGt of pexpr * pexpr
  | PEGte of pexpr * pexpr
  | PEEq of pexpr * pexpr
  | PENeq of pexpr * pexpr

  [@@deriving show {with_path = false}]


type declaration = 
  | DOpen of string
  | DDef of iden * ptype option * pexpr * bool
  | DType of iden * ptype (* iden * typedef *)
  | DExternal of iden * ptype * string
[@@deriving show {with_path = false}]


type t = declaration list [@@deriving show {with_path = false}]


(* https://stackoverflow.com/questions/45024211/menhir-associate-ast-nodes-with-token-locations-in-source-file *)
(* type empt = | Decl of t | Expr of pexpr

module LocationTable = Ephemeron.K1.Make(struct
  type t = empt
  let hash = Hashtbl.hash (* or your own hash if you have one *)
  let equal = (=) (* or a specilized equal operator *)
end)  *)


(* locations:   (string, Parse_tree.empt) Ephemeron.K1.t; *)
