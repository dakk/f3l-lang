(* polymorphic ast traversal *)
open Expr

type 'a t_ovverride = texpr -> 'a
type 'a t_join = 'a -> 'a -> 'a

let traverse (te: texpr) (tf: 'a t_ovverride) (jf: 'a t_join) (empty: 'a) = 
  let rec traverse' (t, e) =
    try 
      tf (t, e)
    with | _ -> match e with
  | Record (a) -> 
    List.fold_left (fun acc e -> jf (traverse' @@ snd e) acc) empty a

  | Bool (_)
  | Nat (_)
  | Int (_)
  | Float (_)
  | String (_)
  | Bytes (_)
  | Typed (_)
  | LocalRef (_)
  | Unit 
  | GlobalRef (_)
  | External (_, _)
  | UnionValue (_) -> empty

  | Lambda (_, a)
  | RecordAccess (a, _)
  | PairFst (a)
  | PairSnd (a)
  | Not (a)
    -> traverse' a

  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | And (a, b)
  | Or (a, b)
  | Lt (a, b)
  | Lte (a, b)
  | Gt (a, b)
  | Gte (a, b)
  | Eq (a, b)
  | Neq (a, b)
  | Apply (a, b)
  | LetIn (_, _, a, b)
  | Pair (a, b) -> jf (traverse' a) (traverse' b)

  | IfThenElse (a, b, c) -> jf (jf (traverse' a) (traverse' b)) (traverse' c)
  in traverse' te



(* open Ttype

let rec map_types (t, e) (tf: ttype -> ttype) = match (e) with 
  | Record (a) -> 
    Record (List.map (fun (ii, ttee) -> 
      let nt = (tf (fst ttee), map_types (ttee) tf) in (ii, nt)) a)
    
  | Typed (t,e) -> Typed (tf t, map_types (t,e) tf)
  | External (t, n) -> External (tf t, n)

  | _ -> (t,e) *)
(* 
  | Lambda (_, a)
  | RecordAccess (a, _)
  | PairFst (a)
  | PairSnd (a)
  | Not (a)
    -> traverse' a

  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | And (a, b)
  | Or (a, b)
  | Lt (a, b)
  | Lte (a, b)
  | Gt (a, b)
  | Gte (a, b)
  | Eq (a, b)
  | Neq (a, b)
  | Apply (a, b)
  | LetIn (_, _, a, b)
  | Pair (a, b) -> jf (traverse' a) (traverse' b)

  | IfThenElse (a, b, c) -> jf (jf (traverse' a) (traverse' b)) (traverse' c) *)


