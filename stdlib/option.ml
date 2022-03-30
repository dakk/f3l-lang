module Option = struct 
  type e = Some | None

  type 'a t = (e * 'a)

  let is_none v = if fst v = None then true else false 

  let is_some v = ! is_none v 

  let get_some v = snd v
end