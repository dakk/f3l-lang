open Untyped_ast
open Format
open Helpers.Gen_utils


let rec pp_lexpr fmt (e: uexpr) = 
  let pp_infix2 fmt op a b = fprintf fmt "(%a) %s (%a)" pp_lexpr a op pp_lexpr b in
  
match e with
| UExternal (id) -> 
  fprintf fmt "%s" id

| UGlobalRef (id)
| ULocalRef (id) -> 
  fprintf fmt "%s" id

| UUnionValue (id) -> 
  fprintf fmt "%s" id
  
| UUnit -> 
  fprintf fmt "None"

| UBool (true) -> fprintf fmt "True"
| UBool (false) -> fprintf fmt "False"

| UNat (i) -> 
  fprintf fmt "%d" i

| UInt (i) -> 
  fprintf fmt "%d" i

| UFloat (i) -> 
  fprintf fmt "%f" i

| UString (s) -> 
  fprintf fmt "\"%s\"" s

| UBytes (s) -> 
  fprintf fmt "b\"%s\"" (Bytes.to_string s)
  
| UPair (e1, e2) -> 
  fprintf fmt "[%a,%a]" 
    pp_lexpr e1
    pp_lexpr e2

| ULambda (arg, e) -> 
    fprintf fmt "lambda %s: (@[%a@])"    
      arg
      pp_lexpr e

| URecord (il) -> 
  let pp_rec_as fmt (i, e) = fprintf fmt "%s=%a" i pp_lexpr e in
  fprintf fmt "{ %a }"
    (pp_list "; " pp_rec_as) il
  
| URecordAccess (e, i) -> 
  fprintf fmt "%a.%s" 
    pp_lexpr e 
    i

| UPairFst (e) -> 
  fprintf fmt "%a[0]" pp_lexpr e

| UPairSnd (e) ->
  fprintf fmt "%a[1]" pp_lexpr e


(* aritmetic *) 
| UAdd(a,b) -> 
  pp_infix2 fmt "+" a b
  
| USub(a,b) -> 
  pp_infix2 fmt "-" a b
  
| UMul(a,b) -> 
  pp_infix2 fmt "*" a b
  
| UDiv(a,b) -> 
  pp_infix2 fmt "/" a b

| UMod (a, b) -> 
  pp_infix2 fmt "mod" a b


(* bool *)
| UNot(a) -> 
  fprintf fmt "not (%a)" pp_lexpr a

| UAnd(a,b) -> 
  pp_infix2 fmt "and" a b
  
| UOr(a,b) -> 
  pp_infix2 fmt "or" a b

| ULt (a, b) -> 
  pp_infix2 fmt "<" a b
  
| ULte (a, b) -> 
  pp_infix2 fmt "<=" a b

| UGt (a, b) -> 
  pp_infix2 fmt ">" a b

| UGte (a, b) -> 
  pp_infix2 fmt ">=" a b

| UEq (a, b) -> 
  pp_infix2 fmt "==" a b

| UNeq (a, b) -> 
  pp_infix2 fmt "!=" a b

| UIfThenElse (c, a, b) -> 
  fprintf fmt "(%a if %a else %a)" 
    pp_lexpr a
    pp_lexpr c 
    pp_lexpr b

| UApply(lam, par) -> 
  fprintf fmt "%a (%a)" 
    pp_lexpr lam 
    pp_lexpr par     

| ULetIn (id, e, e2) -> 
  fprintf fmt "%s = @\n%a@@%a " 
    id 
    pp_lexpr e 
    pp_lexpr e2

| _ -> failwith ("Unable to translate to py: " ^ show_uexpr e)
