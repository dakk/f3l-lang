# Expressions

Expression blocks are semicolon separated list of expression. The last expression define the type of the whole expression. 

```text
e1; e2
```

The semicolon let the developer to ignore the result of an expression, but it is only possible for unit expression; the following expression will fail to compile:

```text
12 + 15; // ignored int expression
13
```

```text
let a = 12;
a
```

Which can be written also as:

```text
let a = 12 in a
```

Let is also useful for tuple destructuring:

```text
let a: (int, nat) = (12, 13n);
let b: int = let (x, y) = a in x;
```

