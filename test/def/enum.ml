type a = enum (Padre | Madre)
type b = enum (Zio | Zia)

let c: a = a#Padre
let d: a = a#Madre
let e: b = b#Zio
let f: b = b#Zia
let g: (b * a) = (b#Zia, a#Madre)