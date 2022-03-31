# Types

We can declare a new type alias as follow:

```ocaml
type aNewType = int option;
```

Cast from an any type to a specific one:

```ocaml
([]: int list)
```

# Base Types

## Numerical \(int, float \)

### Int

Int type represents integer numbers; we define an _int_:

```c
let a: int = 12;
```


And negate a _nat_ or _int_ \(the result type is always an int\):

```c
let d1: int = neg(12n);   // = -12
let d2: int = neg(-12);   // = 12
```

### Float

```c
let c: float = 12.;
```


## Bool

Bool type represents a boolean, which has two possible values: _true_ and _false._

```cpp
let aTrue: bool = true
let aFalse: bool = false
```

## Union

Enum type are unit variants; low level are represented as nat, so they are comparable \(only for equality\).

```ocaml
type anEnum = Started | Stopped

let av: anEnum = Started
let b: bool = av = Stopped // false
```

## String

String are sequences of characters.

```ocaml
let a: string = "Hello World"
```

We can get the length of a string with _size:_

```ocaml
let b: nat = a.size()
```

And get a _slice_:

```ocaml
let c: string = a.slice(1, 5)
```

And concat two strings:

```ocaml
let c: string = a.concat(b)
```






# Composed Types


## Pair

You can define a pair as follow:

```ocaml
let apair: (int * string) = (12, "hello")

let ai: int = fst apair
let as: string = snd apair
```



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
