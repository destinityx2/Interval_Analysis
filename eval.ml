open Ast
open Hashtbl

let bool_to_int b = if b then 1 else 0
let bool_of_int i = if i > 0 then true else false

(*
	evaluate expression
*)
let rec compute expr sym_table = 
	(* 
		evaluate logical expressions 
		true  - expr is true
		false - otherwise
	*)
let rec check expr sym_table = 
	let func e1 e2 op = op(compute e1 sym_table) (compute e2 sym_table)
	in  	
	match expr with
		| EQ (e1, e2) -> func e1 e2 (==)
		| NE (e1, e2) -> func e1 e2 (!=)
		| GT (e1, e2) -> func e1 e2 (>)
		| GE (e1, e2) -> func e1 e2 (>=)
		| LT (e1, e2) -> func e1 e2 (<)
		| LE (e1, e2) -> func e1 e2 (<=)
		| AND (e1, e2) -> (&&) (bool_of_int (compute e1 sym_table)) (bool_of_int (compute e2 sym_table))
		| OR (e1, e2)  -> (||) (bool_of_int (compute e1 sym_table)) (bool_of_int (compute e2 sym_table))
		| _ -> bool_of_int (compute expr sym_table)
in

	let func e1 e2 op = op(compute e1 sym_table) (compute e2 sym_table)
	in  
	match expr with
		| NUMBER num -> num
		| VAR var -> Hashtbl.find sym_table var
		| PLUS (e1, e2)  -> func e1 e2 (+)
		| MINUS (e1, e2) -> func e1 e2 (-)
		| MUL (e1, e2)   -> func e1 e2 ( * )
		| DIV (e1, e2)   -> func e1 e2 (/)
		| MODULO (e1, e2)-> func e1 e2 (mod)
		| _ -> bool_to_int (check expr sym_table)

(*
	run interp.
	prog  - list of instructions
	input - list of input values 
*)
let run prog input = 
	let cur_in = ref 0 in
	let sym_table = Hashtbl.create 17 in
	
	let step line = match List.nth prog (line - 1) with
		| SKIP (pc) -> pc + 1
		| ASSIGN (pc, var, expr) -> 
			begin
				let tmp = (compute expr sym_table) in
				Hashtbl.replace sym_table var tmp;
				pc + 1
			end
		| WRITE (pc, expr) ->
			begin
				print_string (string_of_int (compute expr sym_table));
				print_newline ();
				pc + 1
			end
		| READ (pc, var) -> 
			begin
				Hashtbl.replace sym_table var (List.nth input !cur_in);
				cur_in := !cur_in + 1;
				pc + 1 
			end
		| IF (pc, expr, pc') | WHILE (pc, expr, pc') ->
			begin
				if bool_of_int (compute expr sym_table) then pc + 1
				else pc'
			end
		| RIGHTBRACKET (pc, pc') -> pc'
		
	in

	let rec iterate line = 
		let next_line = step line in
		if next_line <= List.length prog then
			iterate next_line
		else 
			begin
				(* print symbol's table *)
				let print_var vr vl = 
				begin
					print_string (vr ^ " " ^ (string_of_int vl)); 
					print_newline ()
				end
				in
				Hashtbl.iter print_var sym_table
			end
	in
		iterate 1
