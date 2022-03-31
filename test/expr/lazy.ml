let a: int lazy = Lazy.of(fun () -> 12n + 43n)

let b: int = Lazy.force(a)
