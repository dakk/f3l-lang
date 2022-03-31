type 'a signal = 'a
type 'a set = 'a -> unit

type 'a event = 'a 
type 'a send = 'a -> unit 

module S = struct 
  let create = fun () -> (unit, fun x -> ())
  let map = fun f -> fun s -> 
end 

module E = struct 
  let create = fun () -> (unit, fun x -> ())
  let map = fun f -> fun s -> 
end 