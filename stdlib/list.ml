module List = struct
    type 'a list = ('a * 'a)

    let empty = ((), ())

    let hd = fun (l: 'a list) -> fst (l)

    let tl = fun (l: 'a list) -> snd (l)

    let length (l: 'a list) = match l with
    | (unit, unit) -> 0
    | (v, ll) -> 1 + length ll 

    let is_empty = fun (x,y) -> x = () && y = ()

    let cons (l: 'a list) (v: 'a) = (v, l)

    let map (f: 'a -> 'b) (l: 'a list) = 
        let mapl (l: a' list) (nl: b' list) =
            match l with 
            | (unit, unit) -> nl
            | (v, ll) -> mapl ll (cons nl (f v))
        in mapl l []


    let filter (f: 'a -> bool) (l: 'a list) = 
        let filterl (l: a' list) =
            match l with 
            | (unit, unit) -> nl
            | (v, ll) -> 
                if f v then 
                    cons v (filterl ll)
                else 
                    filterl ll
        in filterl l