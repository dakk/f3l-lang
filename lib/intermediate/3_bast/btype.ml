type iden = string
[@@deriving show {with_path = false}]

type btype = 
  | TAnnot of btype * iden
  | TAny
  | TUnit
  | TInt
  | TNat
  | TFloat
  | TBool
  | TString
  | TBytes
  | TLambda of btype * btype
  | TList of btype
  | TOption of btype
  | TPair of btype * btype
  | TOr of btype * btype
