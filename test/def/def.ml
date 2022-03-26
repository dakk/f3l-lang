let litNone 	: int option 			= None
let litSome 	: int option 			= Some(12)
let litTuple 	: (int * nat * string)	= (12, 32n, "ciao")
let litList 	: int list 				= [13, 14, 15]
let litLambda	: int -> int			= fun (a: int) -> (2)
let litRec2	: { a: string; b: int } = { a="ciao"; b=12 }
