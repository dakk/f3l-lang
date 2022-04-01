open Ttype

type expr = 
| LocalRef of iden 
| GlobalRef of iden
| External of iden * ttype

| Unit 
| Bool of bool
| Int of int 
| Float of float
| String of string
| Char of char
| Typed of texpr * ttype
| Pair of texpr * texpr
| Lambda of (iden * ttype) * texpr
| UnionValue of iden

(* pair *)
| PairFst of texpr
| PairSnd of texpr

(* aritmetic *)
| Add of texpr * texpr
| Sub of texpr * texpr
| Mul of texpr * texpr
| Div of texpr * texpr
| Mod of texpr * texpr

| FAdd of texpr * texpr
| FSub of texpr * texpr
| FMul of texpr * texpr
| FDiv of texpr * texpr

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

