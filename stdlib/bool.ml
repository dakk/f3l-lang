module Bool = struct 
  type t = bool

  let to_int = fun x -> if x then 1 else 0
  let from_int = fun x -> if x == 0 then false else true

  let to_string = fun x -> if x then "true" else "false"

  let to_float = fun x -> if x then 1.0 else 0.0
end 