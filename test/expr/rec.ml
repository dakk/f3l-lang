


let rec r = fun a -> r (a)

let a =
  let rec c = 
    fun z -> c (z)
  in c (0)