type a = Hello | World | No

let z = Hello
let b = match z with 
| Hello -> true 
| World -> false 
| No -> true

let bb = match z with 
  Hello -> true 
| World -> false 
| No -> true

let c = match 2 with 
| 1 -> "ciao" 
| 2 -> "hola"
| _ -> "hello" 

(* let as = Some(12) *)
(* let d = match as.isSome() with | true -> as.getSome() | false -> 13 *)