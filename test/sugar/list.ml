let a = [12;13;14;"String"]


let b = hd (a)
let z = tl (a)

let x = fst (a)
let y = snd (a)


let nl = ("nuovoelemento", a)

let is_empty = fun (l: ('a * 'a)) -> match fst (l) with | () -> true | _ -> false

let rec len = fun x -> match fst (x) with () -> 0 | _ -> 1 + len (snd (x))

let rec map = fun (f, x) -> 
  match x with 
  | () -> ((), ()) 
  | _ -> (f (hd (x)), map (f)(tl (x)))