let a = (12, "string")

let c: int = fst (a)
let d: string = snd (a)


type e = So | No

type 'a t = (e * 'a)

let is_none = fun v: t -> if fst (v) = No then true else false 

let get_some = fun v -> snd (v)

let is_some = fun v -> not (is_none (v))

let aa = (So, 12)
let bb = (No, ())

let aaa = is_some (aa)
let bbb = is_none (bb)