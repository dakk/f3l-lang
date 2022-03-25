open Ttype
open Big_int

let pp_big_int fmt i = 
  Format.fprintf fmt "%s" (string_of_big_int i)

type expr = 
| StorageEntry of iden

| LocalRef of iden 
| StorageRef of iden
| GlobalRef of iden

| None
| Unit 
| Bool of bool
| Nat of big_int 
| Int of big_int 
| String of string
| Bytes of bytes
| Some of texpr
| Typed of texpr * ttype
| List of texpr list 
| Tuple of texpr list
| Lambda of (iden * ttype) list * texpr
| Record of (iden * texpr) list
| EnumValue of (iden)
| RecordAccess of texpr * iden

(* option *)
| OptionGetSome of texpr 
| OptionIsNone of texpr
| OptionIsSome of texpr

(* list *)
| ListEmpty
| ListSize of texpr
| ListPrepend of texpr * texpr
| ListMapWith of texpr * texpr
| ListHead of texpr
| ListTail of texpr
| ListFold of texpr * texpr * texpr
| ListFilter of texpr * texpr

(* string *)
| StringConcat of texpr * texpr 
| StringSlice of texpr * texpr * texpr
| StringSize of texpr

(* bytes *)
| BytesConcat of texpr * texpr 
| BytesSlice of texpr * texpr * texpr
| BytesPack of texpr 
| BytesSize of texpr
| BytesUnpack of texpr

(* tuple *)
| TupleFst of texpr
| TupleSnd of texpr

(* aritmetic *)
| Add of texpr * texpr
| Sub of texpr * texpr
| Mul of texpr * texpr
| Div of texpr * texpr
| Mod of texpr * texpr
| Abs of texpr
| ToInt of texpr
| EDiv of texpr * texpr
| Neg of texpr
| IsNat of texpr

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
| MatchWith of texpr * (texpr * texpr) list
| CaseDefault
| Apply of texpr * texpr

| Copy of texpr 
     
| LetIn of iden * ttype * texpr * texpr
| Let of iden * ttype * texpr 
| LetTuple of (iden * ttype) list * texpr 
| LetTupleIn of (iden * ttype) list * texpr * texpr
| SAssign of iden * texpr
| SRecAssign of iden * iden * texpr 

| Seq of texpr * texpr

[@@deriving show {with_path = false}]

and texpr = (ttype * expr) [@@deriving show {with_path = false}]

