
type 'a t = int -> t


let a: t = fun x -> fun x -> x

let b: t = a (12)