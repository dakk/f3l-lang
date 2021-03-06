let e_sum: int -> int = fun (a: int) -> (0 + 1)
let e_gt: int -> bool = fun (a: int) -> (0 > 1)
let e_gt2: int -> bool = fun (a: int) -> (a > 1)
let e_comp: (int * int) -> bool = fun (a: int * int) -> ((fst (a) * 8) > (snd(a) - 12))
let e_comp2: (int * int) -> bool = fun (a: int * int) -> ((fst(a) * (snd(a) - 8)) > (snd(a) - 12))

(* let e_apply: int -> bool = fun (a: int) -> (e_comp (12, 13)) *)

(* let e_apply2: int -> int = fun (a: int) -> (fun b: int -> b)(12)
let e_apply3: int -> int = fun (a: int) -> e_apply2 (12)

let e_if: int -> int = fun (a: int) -> (if a > 12 then 12 else 14 + 15)

type et = int -> int -> int

let e1: et = fun (a: int) -> (fun (b: int) -> (a + b))
let e2: et = fun (a: int) -> fun (b: int) -> a + b

let e3 = fun a:int -> 12 * a *)


let a = 12 

(* global in lambda *)
let x = fun b -> a * b


let y = 
  let aa = 12 in 
  let y1 = fun b -> aa * b in 
  y1 (12)