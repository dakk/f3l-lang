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
type a = enum (Hello | World | No);
def b = match a#Hello with | a#Hello -> true | a#World -> false;
def c = match 2 with | 1 -> a#Hello | 2 -> a#World | _ -> a#No;

def as = Some(12);
def d = match as.isSome() with | true -> as.getSome() | false -> 13;
```

