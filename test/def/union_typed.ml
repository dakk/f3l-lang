type et = Hello | Ciao of int | Pino of string 

let a: et = Hello
let b: et = Ciao (1)
let c: et = Pino ("ciao")

let z = match b with | Ciao(x) -> x | _ -> 0