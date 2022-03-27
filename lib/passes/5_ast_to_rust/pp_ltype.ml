open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils

let rec pp_ltype fmt (a: ttype) = match a with
| TUnit -> 
  fprintf fmt "unit"

| TInt -> 
  fprintf fmt "int"

| TNat -> 
  fprintf fmt "nat"

| TFloat -> 
  fprintf fmt "float"

| TBool -> 
  fprintf fmt "bool"

| TString -> 
  fprintf fmt "string"

| TBytes -> 
  fprintf fmt "bytes"

| TLambda (p, r) -> 
  fprintf fmt "(%a -> %a)" 
    pp_ltype p
    pp_ltype r

| TUnion (el) -> 
  fprintf fmt "nat"


| TRecord (l) -> 
  let pp_rec_field fmt (x, xt) = fprintf fmt "%s: %a" x pp_ltype xt in
  fprintf fmt "{ @[%a@] }" 
    (pp_list ";@." pp_rec_field) l

| TPair (t1, t2) -> 
  fprintf fmt "(%a * %a)" 
    pp_ltype t1 
    pp_ltype t2

| _ -> raise @@ TypeError (None, sprintf "Type '%s' is not translable to ligo" (show_ttype a))

