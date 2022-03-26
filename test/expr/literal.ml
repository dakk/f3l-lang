let litString: string = "hola"
let litNat: nat = 1n
let litFloat: float = 12.
let litFloat2: float = 12.034
let litInt: int = 12
let litNone: int option = None
let litSome: int option = Some(12)
let litTuple: (int * nat * string) = (12, 32n, "ciao")
let litList: int list = [13, 12, 11]
let litLambda: int -> int	= fun (a: int) -> (2)

let litRec2: { a: string; b: int } = { a="ciao"; b=12 }

type a = enum (Ciao | Mondo)
let b: a = a#Ciao

