open Ast
open Interval
open Hashtbl
open Stack

open Printf

let call_stack = Stack.create ()
let func_tbl : (Ast.var, Ast.instruction list * Ast.var list * Ast.var list) Hashtbl.t = Hashtbl.create 0

(* (func_name, returning approximation) *)
let func_apx = Hashtbl.create 0


let it_to_str (l, h) = Printf.sprintf "[%.1f, %.1f]" l h

(*
	p_len  - instruction list's length
	vars   - list of variables
	arg_lst- list of arg names
*)
let apx p_len vars arg_lst = 
	let add t value var = Hashtbl.replace t var value in
	begin
		let apx = Array.make (p_len+1) (Hashtbl.create 0) in
		(for i = 0 to (Array.length apx) - 1 do 
		begin
			apx.(i) <- Hashtbl.create (List.length vars); 
			List.iter ( add apx.(i) Interval.bot ) vars
		end
		done);
		List.iter ( add apx.(0) Interval.top ) arg_lst;
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
		| FUNC (f_name, expr_list) -> 
		begin
			let (instr_list, var_set, arg_lst) = Hashtbl.find func_tbl f_name in
			try 
				Hashtbl.find func_apx f_name;
			with
				Not_found ->
				begin
				analysis instr_list (apx (List.length instr_list) var_set arg_lst);
				let fa = Stack.pop call_stack in
				Hashtbl.replace func_apx f_name fa;
				fa
				end
		end
		| _ -> (infinity, neg_infinity)	

(*
	compute logical expression
	RESULT:
		(T, F) 
*)
and compute_lg expr sym_table = 
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


and merge st var cr = 
	let pr = Hashtbl.find st var in
	Hashtbl.replace st var (Interval.join cr pr)

and merge' ta tb = 
	Hashtbl.iter (merge tb) ta 

and step prog ref_apx i ret_cnt = match List.nth prog i with
	| ASSIGN (pc, var, expr) ->
		begin
			let j = pc in (* j = pc + 1 *)
			let cr = compute_ar expr !ref_apx.(i) in
			(*Printf.printf "%s\n" (it_to_str cr);*)
			Hashtbl.replace !ref_apx.(i) var (Interval.join cr (Hashtbl.find !ref_apx.(i) var));
			merge' !ref_apx.(i) !ref_apx.(j);
			()
		end
	| WRITE (pc, _) | SKIP (pc) -> merge' !ref_apx.(i) !ref_apx.(i+1)
	| READ (pc, var) ->
		begin
			let j = pc in (* j = pc + 1 *)
			let top = (neg_infinity, infinity) in
			merge' !ref_apx.(i) !ref_apx.(j);
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
			Hashtbl.replace !ref_apx.(k) var f;
		end
	| RIGHTBRACKET (pc, pc') -> merge' !ref_apx.(pc-1) !ref_apx.(pc'-1)
	| RETURN (pc, expr) -> 
		begin
			Stack.push (compute_ar expr !ref_apx.(i)) call_stack;
			merge' !ref_apx.(i) !ref_apx.(i+1);
			ret_cnt := !ret_cnt + 1
		end
(*
	fix-point analysis
	prog	- list of instructions
	fapx	- first approximation 
*)
and analysis prog fapx = 
	(* Kleene iteration *)
	let ret_cnt = ref 0 in
	
	let iterate apx = 
		
		let cur_apx  = ref (Array.create (Array.length apx) apx.(0) ) in
		let prev_apx = ref (Array.create (Array.length apx) apx.(0) ) in
		let fix_pt   = ref false in
		begin
		for i = 0 to (Array.length apx) - 1 do 
		begin
			!prev_apx.(i) <- Hashtbl.copy apx.(i);
			!cur_apx.(i) <-  Hashtbl.copy apx.(i)
		end
		done;
		while not !fix_pt do
		begin
			for i = 0 to ((List.length prog) - 1) do
			begin
				step prog cur_apx i ret_cnt;	
			end
			done;
			print_apx apx it_to_str;
			print_apx !cur_apx it_to_str;
			print_apx !prev_apx it_to_str;
			(* if current apx equal to previous apx *)
			if (!cur_apx = !prev_apx)
			then fix_pt   := true
			else 
				begin
					(*prev_apx := Array.copy !cur_apx*)
					for i = 0 to (Array.length apx) - 1 do 
					begin
					!prev_apx.(i) <- Hashtbl.copy !cur_apx.(i);
					end
					done
				end
		end
		done;
		if !ret_cnt > 0 then 
		begin
			let ret_join = ref Interval.bot in
			for i = 0 to !ret_cnt - 1 do
				ret_join := Interval.join !ret_join (Stack.pop call_stack)
			done;
			Stack.push !ret_join call_stack
		end;
		!cur_apx
		end
	in 
		iterate fapx

