open Ast
open Ast_ttype
open Ast_expr
open Format
open Pp_ltype


let rec enum_index e i ii = match e with 
| [] -> failwith "Enum value not found"
| x::_ when x = i -> ii
| _::xe -> enum_index xe i (ii+1)

let pp_par fmt ((ti, tt): string * ttype) = 
  fprintf fmt "%s: %a" ti pp_ltype tt



let rec pp_lexpr fmt ((_,e): texpr) = 
  let pp_infix2 fmt op a b = fprintf fmt "(%a) %s (%a)" pp_lexpr a op pp_lexpr b in
  
match e with
| External (id, _) -> 
  fprintf fmt "%s" id

| GlobalRef (id)
| LocalRef (id) -> 
  fprintf fmt "%s" id

| UnionValue (id) -> 
  fprintf fmt "%s" id
  
| Unit -> 
  fprintf fmt "null"

| Bool (i) -> 
  fprintf fmt "%b" i

| Int (i) -> 
  fprintf fmt "%d" i

| Float (i) -> 
  fprintf fmt "%f" i

| String (s) -> 
  fprintf fmt "\"%s\"" s

| Bytes (s) -> 
  fprintf fmt "\"%s\"" (Bytes.to_string s)

| Typed (e, t) -> 
  fprintf fmt "(%a) %a" 
    pp_ltype t
    pp_lexpr e
  
| Pair (e1, e2) -> 
  fprintf fmt "{ .fst=%a; .snd=%a; }" 
    pp_lexpr e1
    pp_lexpr e2

| Lambda (arg, e) -> 
    fprintf fmt "lambda (%a, (%a) { return @[%a@]; }"    
      pp_ltype (fst e)
      pp_par arg
      pp_lexpr e


| PairFst (e) -> 
  fprintf fmt "%a.fst" pp_lexpr e

| PairSnd (e) ->
  fprintf fmt "%a.snd" pp_lexpr e


(* aritmetic *) 
| Add(a,b) -> 
  pp_infix2 fmt "+" a b
  
| Sub(a,b) -> 
  pp_infix2 fmt "-" a b
  
| Mul(a,b) -> 
  pp_infix2 fmt "*" a b
  
| Div(a,b) -> 
  pp_infix2 fmt "/" a b

| Mod (a, b) -> 
  pp_infix2 fmt "%" a b


(* bool *)
| Not(a) -> 
  fprintf fmt "! (%a)" pp_lexpr a

| And(a,b) -> 
  pp_infix2 fmt "&&" a b
  
| Or(a,b) -> 
  pp_infix2 fmt "||" a b

| Lt (a, b) -> 
  pp_infix2 fmt "<" a b
  
| Lte (a, b) -> 
  pp_infix2 fmt "<=" a b

| Gt (a, b) -> 
  pp_infix2 fmt ">" a b

| Gte (a, b) -> 
  pp_infix2 fmt ">=" a b

| Eq (a, b) -> 
  pp_infix2 fmt "==" a b

| Neq (a, b) -> 
  pp_infix2 fmt "!=" a b

| IfThenElse (c, a, b) -> 
  fprintf fmt "if (%a) then %a else %a" 
    pp_lexpr c
    pp_lexpr a 
    pp_lexpr b

| Apply(lam, par) -> 
  fprintf fmt "%a (%a)" 
    pp_lexpr lam 
    pp_lexpr par     

| LetIn (id, tt, e, e2) -> 
  fprintf fmt "let %s: %a = @\n%a in @\n%a " 
    id 
    pp_ltype tt
    pp_lexpr e 
    pp_lexpr e2

