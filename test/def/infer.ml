let a = (a: int) => (0 + 1)
let b = (a: int) => (0 > 1)
let c = (a: int) => (a > 1)
let d = (a: int, b: int) => ((a * 8) > (b - 12))
let e = (a: int, b: int) => ((a * (b - 8)) > (b - 12))
let f = (a: int) => (d (12, 13))

let g = (a: int) => (((b: int) => (b))(12))
let h = (a: int) => (g (12) * g (13))

let i = (a: int) => (if a > 12 then 12 else 14 + 15)
let l= true or false
let m = (a: int) => (match a with | 1 -> Some(12) | 2 -> (None: int option))
let u = Unit

let litString = "hola"
let litNat = 1n
let litInt = 12