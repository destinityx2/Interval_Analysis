let bot = (infinity,neg_infinity)
let top = (neg_infinity, infinity)

let join (l,h) (l',h') = (min l l', max h h')

(*
	for binary arithmetic functions
	Example of use:
		func (+.) a b
*)
let func op (l, r) (l', r') = 
	if (l == infinity && r == neg_infinity) || (l' == infinity && r' == neg_infinity) then
		(infinity, neg_infinity) 
	else
		begin
			let cmp a b = if a < b then -1 else if a == b then 0 else 1 in
			let set = List.sort cmp [op l l'; op l r'; op r l'; op r r'] in
			(List.nth set 0, List.nth set 3)
		end


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

(* widening operator *)
let wide_op : float*float -> float*float -> float*float = fun (l,h) (l',h') ->
	if l == infinity && h == neg_infinity then (l',h')
	else if l' == infinity && h' == neg_infinity then (l,h)
	else 
		begin
			let a = if l' < l then neg_infinity else l in
			let b = if h' > h then infinity else h in
			(a, b)
		end

(* narrwing operator *)
let narrow_op : float*float -> float*float -> float*float = fun (l,h) (l',h') ->
	if l == infinity && h == neg_infinity then bot
	else if l' == infinity && h' == neg_infinity then bot
	else 
		begin
			let a = if l == neg_infinity then l' else l in
			let b = if h == infinity then h' else h in
			(a, b)
		end