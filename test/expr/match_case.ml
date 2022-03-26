type a = enum (Hello | World | No)
let b = match a#Hello with | a#Hello -> true | a#World -> false
let c = match 2 with | 1 -> a#Hello | 2 -> a#World | _ -> a#No

let as = Some(12)
let d = match as.isSome() with | true -> as.getSome() | false -> 13