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
    (List.fold_left (fun acc e -> jf (traverse' @@ snd e) acc) empty a)

  | List (a)
  | Tuple (a) -> 
    (List.fold_left (fun acc e -> jf (traverse' e) acc) empty a)

  | MatchWith (a, eel) -> 
    jf (traverse' a)
    (let a,b = List.split eel in 
    jf
      (List.fold_left (fun acc e -> jf (traverse' e) acc) empty a)
      (List.fold_left (fun acc e -> jf (traverse' e) acc) empty b))

  | Bool (_)
  | Nat (_)
  | Int (_)
  | String (_)
  | Bytes (_)
  | Typed (_)
  | LocalRef (_)
  | None
  | Unit 
  | ListEmpty
  | GlobalRef (_)
  | External (_, _)
  | CaseDefault -> empty

  | UnionValue (_, _, a)
  | Lambda (_, a)
  | Some (a)
  | RecordAccess (a, _)
  | OptionGetSome (a) 
  | OptionIsNone (a)
  | OptionIsSome (a)
  | ListSize (a)
  | ListHead (a)
  | ListTail (a)
  | StringSize (a)
  | BytesSize (a)
  | BytesPack (a)
  | BytesUnpack (a)
  | TupleFst (a)
  | TupleSnd (a)
  | Abs (a)
  | Neg (a)
  | IsNat (a)
  | Let (_, _, a)
  | LetTuple (_, a)
  | SAssign (_, a)
  | Not (a)
  | ToInt (a)
  | SRecAssign (_, _, a) -> traverse' a

  | ListPrepend (a, b)
  | ListMapWith (a, b)
  | StringConcat (a, b) 
  | BytesConcat (a, b)
  | ListFilter (a, b)
  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | EDiv (a, b)
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
  | LetTupleIn(_, a, b)
  | Seq (a, b) -> jf (traverse' a) (traverse' b)

  | ListFold (a, b, c)
  | StringSlice (a, b, c)
  | BytesSlice (a, b, c)
  | IfThenElse (a, b, c) -> jf (jf (traverse' a) (traverse' b)) (traverse' c)
  in traverse' te


