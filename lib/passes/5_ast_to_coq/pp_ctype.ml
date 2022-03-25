open Ast
open Ast_ttype
open Helpers.Errors
open Format
open Helpers.Gen_utils

let rec pp_ctype fmt a = match a with
| TBool -> 
  fprintf fmt "Prop"

| TNat -> 
  fprintf fmt "nat"

| TInt -> 
  fprintf fmt "Z"

| TUnit -> 
  fprintf fmt "unit"

| TEnum (el) -> 
  fprintf fmt "nat"

| TOption (t) -> 
  fprintf fmt "%a option" pp_ctype t

(*  
| TString -> 
  fprintf fmt "string"

| TBytes -> 
  fprintf fmt "bytes"

| TLambda (p, r) -> 
  fprintf fmt "(%a -> %a)" 
    pp_ltype p
    pp_ltype r


| TList (t) -> 
  fprintf fmt "%a list" pp_ltype t

| TRecord (l) -> 
  let pp_rec_field fmt (x, xt) = fprintf fmt "%s: %a" x pp_ltype xt in
  fprintf fmt "{ @[%a@] }" 
    (pp_list ";@." pp_rec_field) l
*)

| TTuple (tl) -> 
  fprintf fmt "(%a)" 
    (pp_list " * " pp_ctype) tl

| _ -> raise @@ TypeError (None, sprintf "Type '%s' is not translable to coq" (show_ttype a))