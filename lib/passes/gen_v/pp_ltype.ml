open Ast
open Ast_ttype
open Format
open Helpers.Errors
open Helpers.Gen_utils

let p_defs = ref ([]: string list)

let pdefs_tostring () = List.fold_left (fun a b -> a ^ b ^ "\n") "\n" (!p_defs)



let rec sttype (at: ttype) = match at with 
| TAny -> "any"
| TUnit -> "unit"
| TInt -> "int"
| TFloat -> "float"
| TBool -> "bool"
| TString -> "string"
| TChar -> "char"
| TLambda (p, r) -> "fn" ^ sttype p ^ sttype r
| TUnion (el) -> List.fold_left (fun acc x -> acc ^ x) "" el
| TPair (t1, t2) -> sttype t1 ^ sttype t2
| TTypeRef (i, _) -> i


let name_of_pair t1 t2 = 
  "PairOf" ^ sttype t1 ^ "" ^ sttype t2

let rec pp_ltype fmt (a: ttype) = match a with
| TTypeRef (_, _) -> failwith "not handled"
| TUnion (_) -> failwith "not handled"

| TAny -> 
  fprintf fmt "any"

| TUnit -> 
  fprintf fmt "voidptr"

| TInt -> 
  fprintf fmt "int"

| TFloat -> 
  fprintf fmt "float"

| TBool -> 
  fprintf fmt "bool"

| TString -> 
  fprintf fmt "string"

| TChar -> 
  fprintf fmt "rune"

| TLambda (p, r) -> 
  fprintf fmt "fn (par) ret" 

| TPair (t1, t2) -> 
  p_defs := !p_defs @ 
    [ 
      (fprintf sfmt "struct %s { fst %a snd %a}" 
        (name_of_pair t1 t2) 
        pp_ltype t1
        pp_ltype t2; sget())
    ]
  ; 
  fprintf fmt "%s" (name_of_pair t1 t2)

