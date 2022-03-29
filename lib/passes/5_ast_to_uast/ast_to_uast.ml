open Untyped_ast
open Ast 
open Ast_expr

let rec translate_exp (a: Ast_expr.texpr): Untyped_ast.uexpr = match snd a with 
| Record (a) -> URecord (List.map (fun (i, te) -> (i, translate_exp te)) a)
| Bool (a) -> UBool(a)
| Nat (a) -> UNat(a)
| Int (a) -> UInt(a)
| Float (a) -> UFloat(a)
| String (a) -> UString(a)
| Bytes (a) -> UBytes(a)
| Typed (a, b) -> translate_exp a
| LocalRef (a) -> ULocalRef(a)
| Unit -> UUnit
| GlobalRef (a) -> UGlobalRef(a) 
| External (a, b) -> UExternal(a) 
(* | UnionValue (a) ->  *)
| Lambda (a, b) -> ULambda (fst a, translate_exp b)
| RecordAccess (a, b) -> URecordAccess(translate_exp a, b)
| PairFst (a) -> UPairFst(translate_exp a)
| PairSnd (a) -> UPairSnd(translate_exp a)
| Apply (a, b) -> UApply(translate_exp a, translate_exp b)
| LetIn (a, b, c, d) -> ULetIn(a, translate_exp c, translate_exp d)
| IfThenElse (a, b, c) -> UIfThenElse (translate_exp a, translate_exp b, translate_exp c)
| Not (a) ->  UNot(translate_exp a)
| Add (a, b) -> UAdd(translate_exp a, translate_exp b)
| Sub (a, b) -> USub(translate_exp a, translate_exp b)
| Mul (a, b) -> UMul(translate_exp a, translate_exp b)
| Div (a, b) -> UDiv(translate_exp a, translate_exp b)
| Mod (a, b) -> UMod(translate_exp a, translate_exp b)
| And (a, b) -> UAnd(translate_exp a, translate_exp b)
| Or (a, b) -> UOr(translate_exp a, translate_exp b)
| Lt (a, b) -> ULt(translate_exp a, translate_exp b)
| Lte (a, b) -> ULte(translate_exp a, translate_exp b)
| Gt (a, b) -> UGt(translate_exp a, translate_exp b)
| Gte (a, b) -> UGte(translate_exp a, translate_exp b)
| Eq (a, b) -> UEq(translate_exp a, translate_exp b)
| Neq (a, b) -> UNeq(translate_exp a, translate_exp b)
| Pair (a, b) -> UPair(translate_exp a, translate_exp b)


let rec translate (a: Ast.t): Untyped_ast.t = match a with
| [] -> []
| (ii, External(a, b))::al -> (ii, UExternal(b)) :: translate al
| (ii, Def(b))::al -> (ii, UDef(translate_exp b)) :: translate al