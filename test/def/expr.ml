let a: int -> int = (a: int) => (0 + 1)
let b: int -> bool = (a: int) => (0 > 1)
let c: int -> bool = (a: int) => (a > 1)
let d: (int * int) -> bool = (a: int, b: int) => ((a * 8) > (b - 12))
let e: (int * int) -> bool = (a: int, b: int) => ((a * (b - 8)) > (b - 12))
let f: int -> bool = (a: int) => (d (12, 13))

let g: int -> int = (a: int) => (((b: int) => (b))(12))
let h: int -> int = (a: int) => (g (12) * g (13))

let i: int -> int = (a: int) => (if a > 12 then 12 else 14 + 15)
let l: bool = true or false
let m: int -> (int option) = (a: int) => (match a with | 1 -> Some(12) | 2 -> (None: int option))
let u: unit = Unit