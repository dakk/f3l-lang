let e_sum: int -> int = (a: int) => (0 + 1)
let e_gt: int -> bool = (a: int) => (0 > 1)
let e_gt2: int -> bool = (a: int) => (a > 1)
let e_comp: (int * int) -> bool = (a: int, b: int) => ((a * 8) > (b - 12))
let e_comp2: (int * int) -> bool = (a: int, b: int) => ((a * (b - 8)) > (b - 12))
let e_apply: int -> bool = (a: int) => (e_comp (12, 13))

let e_apply2: int -> int = (a: int) => (((b: int) => (b))(12))
let e_apply3: int -> int = (a: int) => (e_apply2 (12))

let e_if: int -> int = (a: int) => (if a > 12 then 12 else 14 + 15)
let e_matchwith: int -> (int option) = (a: int) => (match a with | 1 -> Some(12) | _ -> (None: int option))