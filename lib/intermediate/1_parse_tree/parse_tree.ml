type iden = string [@@deriving show {with_path = false}]

type ptype = 
  | PTBuiltin of string               (* type name *)
  | PTTuple of ptype list             (* tuple of other types *)
  | PTRecord of (string * ptype) list (* record is (iden * type) list *)
  | PTCont of string * ptype          (* container type * inner_type *)
  | PTEnum of string list
  | PTLambda of ptype * ptype
  [@@deriving show {with_path = false}]

type pexpr =
  | PEUnit
  | PENone
  | PEBool of bool
  | PENat of int 
  | PEInt of int 
  | PEFloat of float
  | PEString of string
  | PEBytes of string
  | PESome of pexpr
  | PEEnum of iden * string
  | PETyped of pexpr * ptype
  | PEList of pexpr list 
  | PETuple of pexpr list
  | PELambda of (iden * ptype) list * pexpr
  | PERecord of (iden * pexpr) list

  | PERef of iden

  (* aritmetic *)
  | PEAdd of pexpr * pexpr
  | PESub of pexpr * pexpr
  | PEMul of pexpr * pexpr
  | PEDiv of pexpr * pexpr
  | PEEDiv of pexpr * pexpr
  | PEMod of pexpr * pexpr

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

  (* ifthenelse expression *)
  | PEIfThenElse of pexpr * pexpr * pexpr 
  | PEMatchWith of pexpr * (pexpr * pexpr) list
  | PECaseDefault

  (* function apply *)
  | PEHt of iden * iden
  | PEDot of pexpr * iden
  | PEApply of pexpr * pexpr list

  | PELetIn of iden * ptype option * pexpr * pexpr
  | PELet of iden * ptype option * pexpr 
  | PELetTuple of (iden * ptype option) list * pexpr 
  | PELetTupleIn of (iden * ptype option) list * pexpr * pexpr

  | PESeq of pexpr * pexpr

  [@@deriving show {with_path = false}]


type declaration = 
  | DOpen of string
  | DDef of iden * ptype option * pexpr
  | DType of iden * ptype
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
