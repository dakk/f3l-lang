open Ast
open Ast_ttype
open Ast_expr

let replace_union_with_nat ast = 
  let rec replace_type t mp = match t with 
  | TTypeRef (_, t) -> replace_type t mp
  | TLambda (t, tt) -> TLambda (replace_type t mp, replace_type tt mp)
  | TUnion (_) -> TNat
  | TRecord (il) -> TRecord (List.map (fun (n, t) -> (n, replace_type t mp)) il)
  | TPair(t1, t2) -> TPair (replace_type t1 mp, replace_type t2 mp)
  | _ -> t
  in

  let rec replace_exp (t,e) mp : texpr = match e with 
  | Record (a) -> replace_type t mp, Record (List.map (fun (n, e) -> (n, replace_exp e mp)) a)
  | Bool (a) -> replace_type t mp, Bool (a)
  | Nat (a) -> replace_type t mp, Nat (a)
  | Int (a) -> replace_type t mp, Int (a)
  | Float (a) -> replace_type t mp, Float (a)
  | String (a) -> replace_type t mp, String (a)
  | Bytes (a) -> replace_type t mp, Bytes (a)
  | Typed (a, b) -> replace_type t mp, Typed (replace_exp a mp, replace_type b mp)
  | LocalRef (a) -> replace_type t mp, LocalRef (a)
  | Unit -> replace_type t mp, Unit
  | GlobalRef (a) -> replace_type t mp, GlobalRef (a)
  | External (a, b) -> replace_type t mp, External (a, replace_type b mp)
  | UnionValue (a) -> TNat, Nat(List.assoc a mp)
  | Lambda (a, b) -> replace_type t mp, Lambda ((fst a, replace_type (snd a) mp), replace_exp b mp)
  | RecordAccess (a, b) -> replace_type t mp, RecordAccess (replace_exp a mp, b)
  | PairFst (a) -> replace_type t mp, PairFst (replace_exp a mp)
  | PairSnd (a) -> replace_type t mp, PairSnd (replace_exp a mp)
  | Apply (a, b) -> replace_type t mp, Apply (replace_exp a mp, replace_exp b mp)
  | LetIn (a, b, c, d) -> replace_type t mp, LetIn (a, replace_type b mp, replace_exp c mp, replace_exp d mp)
  | IfThenElse (a, b, c) -> replace_type t mp, IfThenElse (replace_exp a mp, replace_exp b mp, replace_exp c mp)
  | Not (a) -> replace_type t mp, Not (replace_exp a mp)
  | Add (a, b) -> replace_type t mp, Add (replace_exp a mp, replace_exp b mp)
  | Sub (a, b) -> replace_type t mp, Sub (replace_exp a mp, replace_exp b mp)
  | Mul (a, b) -> replace_type t mp, Mul (replace_exp a mp, replace_exp b mp)
  | Div (a, b) -> replace_type t mp, Div (replace_exp a mp, replace_exp b mp)
  | Mod (a, b) -> replace_type t mp, Mod (replace_exp a mp, replace_exp b mp)
  | And (a, b) -> replace_type t mp, And (replace_exp a mp, replace_exp b mp)
  | Or (a, b) -> replace_type t mp, Or (replace_exp a mp, replace_exp b mp)
  | Lt (a, b) -> replace_type t mp, Lt (replace_exp a mp, replace_exp b mp)
  | Lte (a, b) -> replace_type t mp, Lte (replace_exp a mp, replace_exp b mp)
  | Gt (a, b) -> replace_type t mp, Gt (replace_exp a mp, replace_exp b mp)
  | Gte (a, b) -> replace_type t mp, Gte (replace_exp a mp, replace_exp b mp)
  | Eq (a, b) -> replace_type t mp, Eq (replace_exp a mp, replace_exp b mp)
  | Neq (a, b) -> replace_type t mp, Neq (replace_exp a mp, replace_exp b mp)
  | Pair (a, b) -> replace_type t mp, Pair(replace_exp a mp, replace_exp b mp)
  in 

  let rec ruwn a mp i = match a with 
  | [] -> []
  | (_, Type(TUnion(el)))::al -> 
    let rec replace_un acc l mp i = (match l with
    | [] -> (acc, mp, i)
    | ii::l' -> replace_un (acc) l' ((ii, i)::mp) (i+1))
    (* (ii, Def(TNat, Nat(i))):: *)
    in
    let na, mp', i' = replace_un [] el mp i in 
    (na @ (ruwn al mp' i'))

  | (_, Type(_))::al -> 
    (* (ii, Type(replace_type t mp)):: *)
    ruwn al mp i
  | (ii, Def((t,e)))::al -> (ii, Def(replace_exp (t,e) mp))::ruwn al mp i


  | aa::al -> aa::(ruwn al mp i)
  in ruwn ast [] 1