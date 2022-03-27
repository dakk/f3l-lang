# Composed Types


## Option

Options represents a value that could be defined _Some\(value\)_ or not _None_.

```ocaml
let a: nat option = None
let b: nat option = Some(12n)
```

The option type offers some helpers:

```ocaml
let c: bool = is_none a
let d: bool = is_some a

let e: nat = match b with | Some(v) -> v | None -> 0
```

## List

List type represents list of same-type elements; a list can be declared as follow:

```ocaml
let l: nat list = []
let l1: nat list = [12n, 13n]
```

```ocaml
let lh: nat = hd l
```

```ocaml
let lt: nat list = tl l
```

```ocaml
let l2: nat list = 14n::l
```

```ocaml
let l3: nat list = map (fun (a: nat) -> (a * 2n)) l
```

```ocaml
let l4: nat list = filter (fun (a: nat) -> (a <= 13n)) l
```



## Pair

You can define a pair as follow:

```ocaml
let apair: (int * string) = (12, "hello")

let ai: int = fst apair
let as: string = snd apair
```


## Record


## Lambda

Lambda type represent anonymous functions.

```ocaml
let e_sum: int -> int = fun (a: int) -> (0 + 1)
let e_gt: int -> bool = fun (a: int) -> (0 > 1)
let e_gt2: int -> bool = fun (a: int) -> (a > 1)
let e_comp: (int * int) -> bool = fun (a: int, b: int) -> ((a * 8) > (b - 12))
let e_comp2: (int * int) -> bool = fun (a: int, b: int) -> ((a * (b - 8)) > (b - 12))
let e_apply: int -> bool = fun (a: int) -> (e_comp (12, 13))

let e_apply2: int -> int = fun (a: int) -> ((fun (b: int) -> (b))(12))
let e_apply3: int -> int = fun (a: int) -> (e_apply2 (12))
```
