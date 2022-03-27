open Pair_opt

(* let litNone 	: int option 			= none
let litSome 	: int option 			= some (12) *)
let litPair 	: (int * string)	= (12, "ciao")
let litList 	: int list 				= [13, 14, 15]
let litLambda	: int -> int			= fun (a: int) -> (2)
let litRec2	: { a: string; b: int } = { a="ciao"; b=12 }
