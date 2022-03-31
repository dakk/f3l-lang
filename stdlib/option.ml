module Option = struct 
  type e = Some | None
  
  type 'a option = (e * 'a)
  
  let none = (None, ())
  let some = fun v -> (Some, v) 
  
  let is_none = fun (v: option) -> if fst (v) = None then true else false 
  
  let get_some = fun v -> snd (v)
  
  let is_some = fun v -> not (is_none (v))
end

