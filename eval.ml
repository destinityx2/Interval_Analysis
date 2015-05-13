open Ast
open Hashtbl
open Stack

let call_stack = Stack.create ()
let func_tbl : (Ast.var, Ast.instruction list * Ast.var list * Ast.var list) Hashtbl.t = Hashtbl.create 0

let cur_in = ref 0
let input = ref []

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
		| FUNC (f_name, expr_list) -> 
		begin
			let expr_val_lst = ref [] in
			let f e = expr_val_lst := !expr_val_lst @ [compute e sym_table] in
			List.iter f expr_list;
			call f_name !expr_val_lst;
			Stack.pop call_stack
		end
		| _ -> bool_to_int (check expr sym_table)
		
and call f_name arg_val = 
	let sym_table = Hashtbl.create 17 in
	let (prog, var_set, arg_names) = Hashtbl.find func_tbl f_name in
	
	begin
		for i = 0 to (List.length arg_val) - 1 do
			Hashtbl.replace sym_table (List.nth arg_names i) (List.nth arg_val i)
		done;
		iterate prog sym_table 1
	end
	
and  iterate prog sym_table line = 
	let (next_line, cont) = step (List.nth prog (line - 1)) sym_table in
	if next_line <= List.length prog && cont then
		iterate prog sym_table next_line
		
and step instr sym_table = match instr with
		| SKIP (pc) -> (pc + 1, true)
		| ASSIGN (pc, var, expr) -> 
			begin
				let tmp = (compute expr sym_table) in
				Hashtbl.replace sym_table var tmp;
				(pc + 1,true)
			end
		| WRITE (pc, expr) ->
			begin
				print_string (string_of_int (compute expr sym_table));
				print_newline ();
				(pc + 1,true)
			end
		| READ (pc, var) -> 
			begin
				Hashtbl.replace sym_table var (List.nth !input !cur_in);
				cur_in := !cur_in + 1;
				(pc + 1,true) 
			end
		| IF (pc, expr, pc') | WHILE (pc, expr, pc') ->
			begin
				if bool_of_int (compute expr sym_table) then (pc + 1,true)
				else (pc',true)
			end
		| RIGHTBRACKET (pc, pc') -> (pc',true)
		| RETURN (pc, expr) -> 
			begin
				Stack.push (compute expr sym_table) call_stack;
				(pc + 1,false)
			end

	