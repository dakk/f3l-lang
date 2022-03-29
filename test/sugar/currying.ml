let a = fun (x: int, y: int) -> x + y

let b : int -> int -> int = a

let z: int -> int = b (1)

let y: int = z (2)

let x1 = 13 |> (12 |> a)

(* this is not handled *)
(* let x = a(12,13) *)