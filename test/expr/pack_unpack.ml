let a: bytes = Bytes.pack (12n)

let b: nat option = Bytes.unpack (a)
let c = (Bytes.unpack (a) : nat option)
let c2: nat option = Bytes.unpack (a)

let lp: nat -> nat = (a1: nat) => (a1*2n)
let plp = Bytes.pack (lp)
let lp2: (nat -> nat) option = Bytes.unpack (plp)