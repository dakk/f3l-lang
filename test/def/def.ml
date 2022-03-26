def litNone 	: int option 			= None
def litSome 	: int option 			= Some(12)
def litTuple 	: (int * nat * string)	= (12, 32n, "ciao")
def litList 	: int list 				= [13, 14, 15]
def litLambda	: int -> int			= fun (a: int) -> (2)
def litRec2	: { a: string; b: int } = { a="ciao"; b=12 }
