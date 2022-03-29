open Ast
open Ast_ttype
open Format
open Helpers.Gen_utils

let rec pp_ltype fmt (a: ttype) = match a with
| TTypeRef (i, t) -> fprintf fmt "WTF"
| TAny -> fprintf fmt "void *"
| TUnion (l) -> fprintf fmt "WTF"

| TUnit -> 
  fprintf fmt "void *"

| TInt -> 
  fprintf fmt "int"

| TNat -> 
  fprintf fmt "unsigned"

| TFloat -> 
  fprintf fmt "float"

| TBool -> 
  fprintf fmt "unsigned short"

| TString -> 
  fprintf fmt "char []"

| TBytes -> 
  fprintf fmt "char []"

| TLambda (p, r) -> 
  fprintf fmt "%a (*)(%a)" 
    pp_ltype r
    pp_ltype p

| TRecord (l) -> 
  let pp_rec_field fmt (x, xt) = fprintf fmt "%s: %a" x pp_ltype xt in
  fprintf fmt "{ @[%a@] }" 
    (pp_list ";@." pp_rec_field) l

| TPair (t1, t2) -> 
  fprintf fmt "pair_of_%a_%a" 
    pp_ltype t1 
    pp_ltype t2


