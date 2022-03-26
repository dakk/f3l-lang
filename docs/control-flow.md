# Control flow

## If then else

```text
if (a = 12n) then (15) else (12)
```

## Pattern matching

```ocaml
match a with | 12 -> true | _ -> false
```

```ocaml
type a = Hello | World | No
let b = match Hello with | Hello -> true | World -> false
let c = match 2 with | 1 -> Hello | 2 -> World | _ -> a#No

let as = Some(12)
let d = match as.isSome() with | true -> as.getSome() | false -> 13
```

