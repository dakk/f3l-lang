let a: int -> int = fun (a: int) -> (0 + 1)
let b: int -> bool = fun (a: int) -> (0 > 1)
let c: int -> bool = fun (a: int) -> (a > 1)

(* let f: int -> bool = fun (a: int) -> (d (12, 13)) *)

let g: int -> int = fun (a: int) -> ((fun (b: int) -> (b))(12))
let h: int -> int = fun (a: int) -> (g (12) * g (13))

let i: int -> int = fun (a: int) -> (if a > 12 then 12 else 14 + 15)
let l: bool = true || false
let u: unit = ()