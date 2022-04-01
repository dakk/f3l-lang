open Ast
open Ast_ttype
open Format

let rec pp_ltype fmt (a: ttype) = match a with
| TTypeRef (_, _) -> failwith "not handled"
| TUnion (_) -> failwith "not handled"


| TAny -> 
  fprintf fmt "void *"

| TUnit -> 
  fprintf fmt "void *"

| TInt -> 
  fprintf fmt "int"

| TFloat -> 
  fprintf fmt "float"

| TBool -> 
  fprintf fmt "unsigned short"

| TString -> 
  fprintf fmt "char []"

| TChar -> 
  fprintf fmt "char"

| TLambda (p, r) -> 
  fprintf fmt "%a (*)(%a)" 
    pp_ltype r
    pp_ltype p

| TPair (t1, t2) -> 
  fprintf fmt "pair_of_%a_%a" 
    pp_ltype t1 
    pp_ltype t2


