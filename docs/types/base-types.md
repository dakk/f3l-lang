# Base Types

## Numerical \(nat, int, float \)

### Nat

Nat type represents natural numbers \(positive integers U { 0 }\); a nat literal should be always followed by an _n._

```c
let a: nat = 12n;
```

A nat value can be converted to _int_ type using the _int\(nat\)_ builtin function:

```c
let b: int = int(12n);
```

### Int

Int type represents integer numbers; we define an _int_:

```c
let a: int = 12;
```

We can check if the _int_ is a _nat_:

```c
let b1: bool = isNat(12);  // true
let b2: bool = isNat(-12); // false
```

Transform an _int_ to a _nat_:

```c
let c: nat = abs(-12);
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
let aTrue: bool = true;
let aFalse: bool = false;
```

## Union

Enum type are unit variants; low level are represented as nat, so they are comparable \(only for equality\).

```cpp
type anEnum = Started | Stopped;

let av: anEnum = Started;
let b: bool = av = Stopped; // false
```

## String

String are sequences of characters.

```cpp
let a: string = "Hello World";
```

We can get the length of a string with _size:_

```cpp
let b: nat = a.size();
```

And get a _slice_:

```cpp
let c: string = a.slice(1, 5);
```

And concat two strings:

```cpp
let c: string = a.concat(b);
```

## Bytes

Bytes are sequences of bytes. Like strings you can get the _length_ and a _slice_. 

```cpp
let a: bytes = "...";
let b: nat = a.size();
let c: bytes = a.slice(1, 5);
```

Bytes type is useful for encoding/decoding f3l expressions using _pack_ and _unpack_:

```cpp
let n: nat = 12n;
let a: bytes = Bytes.pack (n); // pack a nat
let b: nat option = (Bytes.unpack (a): nat option);

let c: bool = n = (Option.getSome(b));
```

And concat two bytes:

```cpp
let c: bytes = a.concat(b);
```

## Unit

The unit type is a type which has only a value _Unit._

```cpp
let a: unit = Unit;
```



