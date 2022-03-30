
module Module_a = struct 
  type t = 'a -> 'a

  let c = 12

  let foo = fun x -> x * 2
end 

let bar: Module_a.t = Module_a.foo