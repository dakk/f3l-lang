let a = fun (a: int) -> (0 + 1)
let b = fun (a: int) -> (0 > 1)
let c = fun (a: int) -> (a > 1)
let d = fun (a: int, b: int) -> ((a * 8) > (b - 12))
let e = fun (a: int, b: int) -> ((a * (b - 8)) > (b - 12))
let f = fun (a: int) -> (d (12, 13))

let g = fun (a: int) -> ((fun (b: int) -> (b))(12))
let h = fun (a: int) -> (g (12) * g (13))

let i = fun (a: int) -> (if a > 12 then 12 else 14 + 15)
let l= true or false
let m = fun (a: int) -> (match a with | 1 -> Some(12) | 2 -> (None: int option))
let u = ()

let litString = "hola"
let litNat = 1n
let litInt = 12