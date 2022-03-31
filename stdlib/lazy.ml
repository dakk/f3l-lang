module Lazy = struct 
  type 'a lazy = unit -> 'a

  let force = fun x -> x ()

  let from_fun = fun f: (unit -> 'a) -> f
  let from_val = fun x -> lazy (fun () -> x)
  let map = fun f -> fun x -> f (force (x))
end