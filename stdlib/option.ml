type 'a t = Some of 'a | None;

let is_some v =
    match v with
    | Some(vv) -> true 
    | None -> false


let is_none v = not is_some v


