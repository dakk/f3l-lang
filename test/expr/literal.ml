open Pair_opt

let litString: string = "hola"
let litNat: nat = 1n
let litFloat: float = 12.
let litFloat2: float = 12.034
let litInt: int = 12
(* let litNone: int option = none *)
(* let litSome: int option = some (12) *)
let litPair: (int * string) = (12, "ciao")
let litList: int list = [13, 12, 11]
let emptList: int list = []
let litLambda: int -> int = fun a: int -> 2
let litRec2: { a: string; b: int } = { a="ciao"; b=12 }

type a = Ciao | Mondo
let b: a = Ciao

