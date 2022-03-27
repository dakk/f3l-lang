type e = Some | None

type 'a t = (e * 'a)

let is_none v = if fst v = None then true else false 

let is_some v = not is_none v 

let get_some v = snd v