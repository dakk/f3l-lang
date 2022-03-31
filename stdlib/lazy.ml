module Lazy = struct 
  type 'a lazy = unit -> 'a

  let force = fun x -> x ()

  let lazy = fun f -> fun () -> f ()
end