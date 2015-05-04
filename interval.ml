
let leq (l,h) (l',h') = l' <= l && h <= h'

let join (l,h) (l',h') = (min l l', max h h')

let incr : float*float -> float*float = fun (l, h) -> (l +. 1.0, h +. 1.0)

let decr : float*float -> float*float = fun (l, h) -> (l -. 1.0, h -. 1.0)

let iszero : float*float -> float*float = fun (l, h) -> 
	if l <= 0.0 && 0.0 <= h then (0.0,0.0) else (infinity, neg_infinity)

let notzero : float*float -> float*float = fun (l, h) -> 
	if l > 0.0 || h < 0.0 || (l < 0.0 && 0.0 < h) 
	then (l, h)
	else if h == 0.0 then (l, h -. 1.0)
	else (l +. 1.0, h)
