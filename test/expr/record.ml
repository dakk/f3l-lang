type r = { a: int; b: { c: int }};

let a: r = { a=12; b={ c=13 } };
let c: int = a.b.c;