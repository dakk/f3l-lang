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

let c = match 2 with | 1 -> Hello | 2 -> World | _ -> No

let as = Some(12)
(* let d = match as.isSome() with | true -> as.getSome() | false -> 13 *)