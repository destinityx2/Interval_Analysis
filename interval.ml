let bot = (infinity,neg_infinity)

let join (l,h) (l',h') = (min l l', max h h')

(*
	for binary arithmetic functions
	Example of use:
		func (+.) a b
*)
let func op (l, r) (l', r') = 
	let cmp a b = if a < b then -1 else if a == b then 0 else 1 in
	let set = List.sort cmp [op l l'; op l r'; op r l'; op r r'] in
	(List.nth set 0, List.nth set 3)


(*
	binary logical functions
*)

(* == *)
let eq (l, r) (l', r') = 
	if l > r' || r < l' then (infinity,neg_infinity)
	else (max l l', min r r')
	
(* <= *)
let le (l, r) (l', r') =
	if l <= l' then (l, min r l')
	else (infinity, neg_infinity)

(* < *)
let lt (l, r) (l', r') = 
	if l < l' then (l, min r (l' -. 1.0))
	else (infinity, neg_infinity)
	
(* >= *)
let ge (l, r) (l', r') = 
	if r >= r' then (max l r', r)
	else (infinity, neg_infinity)

(* > *)
let gt (l, r) (l', r') = 
	if r > r' then (max l (r' +. 1.0), r)
	else (infinity, neg_infinity)

(* != *)
let ne i i' = 
	let e = eq i i' in
	join (lt i e) (gt i e)

