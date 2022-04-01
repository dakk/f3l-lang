open Ast
open Ast_ttype
open Format

let p_defs = ref ([]: string list)

let pdefs_tostring () = List.fold_left (^) "" (!p_defs)

let rec pp_ltype fmt (a: ttype) = match a with
| TTypeRef (_, _) -> failwith "not handled"
| TUnion (_) -> failwith "not handled"


| TAny -> 
  fprintf fmt "auto"

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
  fprintf fmt "auto" 

| TPair (t1, t2) -> 
  p_defs := !p_defs @ [ "typedef struct { " ^ show_ttype t1 ^ " fst; " ^ show_ttype t2 ^ " snd; } pair_of_" ^ show_ttype t1 ^ "_" ^ show_ttype t2 ^ ";" ]
  ; 
  fprintf fmt "pair_of_%s_%s" 
    (show_ttype t1)
    (show_ttype t2)

