# Composed Types

## Option

Options represents a value that could be defined _Some\(value\)_ or not _None_.

```csharp
def a: nat option = None;
def b: nat option = Some(12n);
```

The option type offers some helpers:

```csharp
def c: bool = isNone(a); // returns true if a is None
def d: bool = isSome(a); // returns true if a is Some(_)
def e: nat = getSome(b): // extract the value (if any) or fail
```

## List

List type represents list of same-type elements; a list can be declared as follow:

```csharp
let l: nat list = (List.empty(): nat list); // empty list
let l1: nat list = [12n, 13n]; // literal list
```

```csharp
let ls: nat = l.size();
```

```csharp
let lh: nat = l.head();
```

```csharp
let lt: nat list = l.tail();
```

```csharp
let l2: nat list = l.prepend(14n);
```

```csharp
let l3: nat list = l.mapWith((a: nat) => (a * 2n));
```

```csharp
let l4: nat list = l.filter((a: nat) => (a <= 13n));
```

{% hint style="info" %}
_prepend_, _mapWith_ and _filter_ are applied in-place if used over storage fields. Their types are _unit_ instead of _nat list._ To apply as expression, surround the storage field with _copy\(\)_.
{% endhint %}


## Tuple

A tuple is a pair, triple, etc of different types; you can define a tuple as follow:

```csharp
let atuple: (int, string) = (12, "hello");
```

## Record

## Map & Big\_map

\('a, 'b\) Map and \('a, 'b\) Big\_map are collections of _'b_ indexed by a key _'a_; _'a_ should be a comparable.

{% hint style="info" %}
_update, remove, mapWith, fold, filter_ are applied in-place if used over storage fields. Their types are _unit_ instead of _nat set._ To apply as expression, surround the storage field with _copy\(\)_.
{% endhint %}


## Lambda

Lambda type represent anonymous functions.

```csharp
def e_sum: int -> int = (a: int) => (0 + 1);
def e_gt: int -> bool = (a: int) => (0 > 1);
def e_gt2: int -> bool = (a: int) => (a > 1);
def e_comp: (int, int) -> bool = (a: int, b: int) => ((a * 8) > (b - 12));
def e_comp2: (int, int) -> bool = (a: int, b: int) => ((a * (b - 8)) > (b - 12));
def e_apply: int -> bool = (a: int) => (e_comp (12, 13));

def e_apply2: int -> int = (a: int) => (((b: int) => (b))(12));
def e_apply3: int -> int = (a: int) => (e_apply2 (12));
```
