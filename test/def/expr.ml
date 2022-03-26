let a: int -> int = fun (a: int) -> (0 + 1)
let b: int -> bool = fun (a: int) -> (0 > 1)
let c: int -> bool = fun (a: int) -> (a > 1)
let d: (int * int) -> bool = fun (a: int, b: int) -> ((a * 8) > (b - 12))
let e: (int * int) -> bool = fun (a: int, b: int) -> ((a * (b - 8)) > (b - 12))
let f: int -> bool = fun (a: int) -> (d (12, 13))

let g: int -> int = fun (a: int) -> ((fun (b: int) -> (b))(12))
let h: int -> int = fun (a: int) -> (g (12) * g (13))

let i: int -> int = fun (a: int) -> (if a > 12 then 12 else 14 + 15)
let l: bool = true or false
let m: int -> (int option) = fun (a: int) -> (match a with | 1 -> Some(12) | 2 -> (None: int option))
let u: unit = Unit