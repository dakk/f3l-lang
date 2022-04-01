
type 'a t = int -> t

let a: t = fun x -> fun x -> x

let b: t = a (12)

let c = (12, "strn")

let cc = (15.0, c)

let res = fst (c)