open Pair_opt

let litString = "hola"
let litNat = 1n
let litInt = 12
let litSome = some (12)
let litPair = (12, "ciao")
let litList = [13, 14, 15]
let litLambda	= fun (a: int) -> (2)
let litRec2 = { a="ciao"; b=12 }

type a = Ciao | Mondo
let b = Ciao