type iden = string
[@@deriving show {with_path = false}]

type uexpr = 
| ULocalRef of iden 
| UGlobalRef of iden
| UExternal of iden

| UUnit 
| UBool of bool
| UInt of int 
| UFloat of float
| UString of string
| UBytes of bytes
| UPair of uexpr * uexpr
| ULambda of iden * uexpr
| UUnionValue of iden

(* pair *)
| UPairFst of uexpr
| UPairSnd of uexpr

(* aritmetic *)
| UAdd of uexpr * uexpr
| USub of uexpr * uexpr
| UMul of uexpr * uexpr
| UDiv of uexpr * uexpr
| UMod of uexpr * uexpr


(* bool *)
| UAnd of uexpr * uexpr
| UOr of uexpr * uexpr
| UNot of uexpr
| ULt of uexpr * uexpr
| ULte of uexpr * uexpr
| UGt of uexpr * uexpr
| UGte of uexpr * uexpr
| UEq of uexpr * uexpr
| UNeq of uexpr * uexpr

| UIfThenElse of uexpr * uexpr * uexpr 
| UApply of uexpr * uexpr     
| ULetIn of iden * uexpr * uexpr

[@@deriving show {with_path = false}]


type ue = UDef of uexpr | UExternal of iden [@@deriving show {with_path = false}]

type t = (iden * ue) list [@@deriving show {with_path = false}]
