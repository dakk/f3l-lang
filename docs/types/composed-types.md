# Composed Types

## Union types

```ocaml
type t = Int of int | Nat of nat | Nil of unit;
```

## Option

Options represents a value that could be defined _Some\(value\)_ or not _None_.

```ocaml
def a: nat option = None;
def b: nat option = Some(12n);
```

The option type offers some helpers:

```ocaml
def c: bool = Option.is_none a; // returns true if a is None
def d: bool = Option.is_some a; // returns true if a is Some(_)

def e: nat = match b with | Some(v) -> v | None -> 0
```

## List

List type represents list of same-type elements; a list can be declared as follow:

```csharp
let l: nat list = (List.empty(): nat list); // empty list
let l: nat list = []; // empty list
let l1: nat list = [12n, 13n]; // literal list
```

```csharp
let ls: nat = List.size l;
```

```csharp
let lh: nat = List.head l;
```

```csharp
let lt: nat list = List.tail l;
```

```csharp
let l2: nat list = l.prepend(14n);
```

```csharp
let l3: nat list = List.map ((a: nat) => (a * 2n)) l;
```

```csharp
let l4: nat list = List.filter ((a: nat) => (a <= 13n)) l;
```



## Tuple

A tuple is a pair, triple, etc of different types; you can define a tuple as follow:

```ocaml
let atuple: (int, string) = (12, "hello");

let ai: int = fst atuple;
let as: string = snd atuple;
```

## Record


## Lambda

Lambda type represent anonymous functions.

```ocaml
def e_sum: int -> int = (a: int) => (0 + 1);
def e_gt: int -> bool = (a: int) => (0 > 1);
def e_gt2: int -> bool = (a: int) => (a > 1);
def e_comp: (int, int) -> bool = (a: int, b: int) => ((a * 8) > (b - 12));
def e_comp2: (int, int) -> bool = (a: int, b: int) => ((a * (b - 8)) > (b - 12));
def e_apply: int -> bool = (a: int) => (e_comp (12, 13));

def e_apply2: int -> int = (a: int) => (((b: int) => (b))(12));
def e_apply3: int -> int = (a: int) => (e_apply2 (12));
```
