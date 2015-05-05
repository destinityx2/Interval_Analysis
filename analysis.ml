open Ast
open Interval
open Hashtbl

let it_to_str (l, h) = Printf.sprintf "[%.1f, %.1f]" l h

let apx p_len vars = 
	let add t var = Hashtbl.replace t var Interval.bot in
	begin
		let apx = Array.make (p_len+1) (Hashtbl.create 0) in
		(for i = 0 to (Array.length apx) - 1 do 
		begin
			apx.(i) <- Hashtbl.create 17;
			List.iter ( add apx.(i) ) vars
		end
		done);
		apx
	end

let print_apx apx domain_to_str = 
	for i = 0 to (Array.length apx) - 2 do
	begin
		Printf.printf "%i: " (i+1);
		let print_var_apx var apx =
			Printf.printf "%s %s\t" var (domain_to_str apx)
		in
		Hashtbl.iter print_var_apx apx.(i);
		Printf.printf "\n"
	end
	done	

(* 
	compute arithmetic expression
*)
let rec compute_ar expr sym_table = 

	let func e1 e2 op = op(compute_ar e1 sym_table) (compute_ar e2 sym_table)
	in  	
 
	match expr with
		| NUMBER num     -> (float_of_int num, float_of_int num)
		| VAR var        -> Hashtbl.find sym_table var
		| PLUS (e1, e2)  -> func e1 e2 (Interval.func (+.))
		| MINUS (e1, e2) -> func e1 e2 (Interval.func (-.))
		| MUL (e1, e2)   -> func e1 e2 (Interval.func ( *.))
		| DIV (e1, e2)   -> func e1 e2 (Interval.func (/.))
		| MODULO (e1, e2)-> func e1 e2 (Interval.func (mod_float))
		| _ -> (infinity, neg_infinity)	

(*
	compute logical expression
	RESULT:
		(T, F) 
*)
let compute_lg expr sym_table = 
	let var e = match e with
		| EQ (VAR v, e') | NE (VAR v, e') | GT (VAR v, e')
		| GE (VAR v, e') | LT (VAR v, e') | LE (VAR v, e')
		  -> v
		| _ -> ""
	in
	
	let func e1 e2 op = op(compute_ar e1 sym_table) (compute_ar e2 sym_table)
	in 
	
	let pp = 
	match expr with 
		| EQ (e1, e2) -> (func e1 e2 (Interval.eq), func e1 e2 (Interval.ne))
		| NE (e1, e2) -> (func e1 e2 (Interval.ne), func e1 e2 (Interval.eq))
		| GT (e1, e2) -> (func e1 e2 (Interval.gt), func e1 e2 (Interval.le))
		| GE (e1, e2) -> (func e1 e2 (Interval.ge), func e1 e2 (Interval.lt))
		| LT (e1, e2) -> (func e1 e2 (Interval.lt), func e1 e2 (Interval.ge))
		| LE (e1, e2) -> (func e1 e2 (Interval.le), func e1 e2 (Interval.gt))
		| _ -> (Interval.bot, Interval.bot)
	in
		(var expr, pp)


let merge st var cr = 
	let pr = Hashtbl.find st var in
	Hashtbl.replace st var (Interval.join cr pr)

let merge' ta tb = 
	Hashtbl.iter (merge tb) ta 

let step prog ref_apx i = match List.nth prog i with
	| ASSIGN (pc, var, expr) ->
		begin
			let j = pc in (* j = pc + 1 *)
			let cr = compute_ar expr !ref_apx.(i) in
			Hashtbl.replace !ref_apx.(i) var cr;
			merge' !ref_apx.(i) !ref_apx.(j);
			()
		end
	| WRITE (pc, _) | SKIP (pc) -> merge' !ref_apx.(i) !ref_apx.(i+1)
	| READ (pc, var) ->
		begin
			let j = pc in (* j = pc + 1 *)
			let top = (neg_infinity, infinity) in
			Hashtbl.replace !ref_apx.(i) var top;
			Hashtbl.replace !ref_apx.(j) var top
		end 
	| IF (pc, expr, pc') | WHILE (pc, expr, pc') -> 
		begin
			let j = pc in
			let k = pc' - 1 in
			let (var,(t,f)) = compute_lg expr !ref_apx.(i) in
			merge' !ref_apx.(i) !ref_apx.(j);
			Hashtbl.replace !ref_apx.(j) var t;
			merge' !ref_apx.(i) !ref_apx.(k);
			Hashtbl.replace !ref_apx.(k) var f
		end
	| RIGHTBRACKET (pc, pc') -> merge' !ref_apx.(pc-1) !ref_apx.(pc'-1)
	
(*
	fix-point analysis
	prog	- list of instructions
	fapx	- first approximation 
*)
let analysis prog fapx = 
	(* Kleene iteration *)
	let iterate apx = 
		
		let cur_apx  = ref (Array.copy apx) in
		let prev_apx = ref (Array.copy apx) in
		let fix_pt   = ref false in
		while not !fix_pt do
		begin
			for i = 0 to ((List.length prog) - 1) do
			begin
				step prog cur_apx i;
				(*print_apx !cur_apx it_to_str;
				Printf.printf "\n"*)	
			end
			done;
			(* if current apx equal to previous apx *)
			if (!cur_apx = !prev_apx)
			then fix_pt   := true
			else prev_apx := Array.copy !cur_apx
		end
		done; 
		!cur_apx
	in 
		iterate fapx

