let a: nat lazy = Lazy.of(fun () -> 12n + 43n)

let b: nat = Lazy.force(a)
