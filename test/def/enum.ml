type a = enum (Padre | Madre)
type b = enum (Zio | Zia)
let c: a = Padre
let d: a = Madre
let e: b = Zio
let f: b = Zia
let g: (b * a) = (Zia, Madre)