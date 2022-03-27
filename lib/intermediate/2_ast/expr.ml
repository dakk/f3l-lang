open Ttype


type expr = 
| LocalRef of iden 
| GlobalRef of iden
| External of iden * ttype

| Unit 
| Bool of bool
| Nat of int 
| Int of int 
| Float of float
| String of string
| Bytes of bytes
| Typed of texpr * ttype
| Pair of texpr * texpr
| Lambda of (iden * ttype) list * texpr
| Record of (iden * texpr) list
| UnionValue of iden
| RecordAccess of texpr * iden



(* string *)
(* | StringConcat of texpr * texpr 
| StringSlice of texpr * texpr * texpr
| StringSize of texpr *)

(* bytes *)
(* | BytesConcat of texpr * texpr 
| BytesSlice of texpr * texpr * texpr
| BytesPack of texpr 
| BytesSize of texpr
| BytesUnpack of texpr *)

(* | Abs of texpr
| ToInt of texpr
| Neg of texpr
| IsNat of texpr *)

(* pair *)
| PairFst of texpr
| PairSnd of texpr

(* aritmetic *)
| Add of texpr * texpr
| Sub of texpr * texpr
| Mul of texpr * texpr
| Div of texpr * texpr
| Mod of texpr * texpr
| EDiv of texpr * texpr


(* bool *)
| And of texpr * texpr
| Or of texpr * texpr
| Not of texpr
| Lt of texpr * texpr
| Lte of texpr * texpr
| Gt of texpr * texpr
| Gte of texpr * texpr
| Eq of texpr * texpr
| Neq of texpr * texpr

| IfThenElse of texpr * texpr * texpr 
| Apply of texpr * texpr
     
| LetIn of iden * ttype * texpr * texpr


[@@deriving show {with_path = false}]

and texpr = (ttype * expr) [@@deriving show {with_path = false}]

