open Ast
open Parser
open Lexer
open Printf
open Analysis

open Printf


let run chn trace interval eval input_arr =
		
	let lexbuf = Lexing.from_channel chn in
	let hasht = Parser.program Lexer.token lexbuf in
  	begin 
  	if trace then
		let (instr_lst, var_set, arg_list) = Hashtbl.find hasht "main" in
  		Ast.print_instructions instr_lst
  	else if interval then 
  	begin
  		(* run interval analysis *)
  		(*let res = 
  		  Analysis.analysis prog (Analysis.apx (List.length prog) var_set)
  		in
  		Analysis.print_apx res Analysis.it_to_str*)
  	end
  	else if eval then
	begin
  		let fl k v = Hashtbl.replace Eval.func_tbl k v in
		Hashtbl.iter fl hasht;
		Eval.input := input_arr;
		Eval.call "main" []
	end
  	end


let _ =

    let trace = ref false in
	let interval = ref false in	
	let eval = ref false in
	let inch = open_in Sys.argv.(1) in  (* input file *)
	let input_arr = ref [] in           (* input values *)
	
	begin
		(match Sys.argv.(2) with
			| "-eval" -> eval := true
			| "-interval" -> interval := true
			| "-trace" -> trace := true
			| _ -> exit 0
		);
		(for i = 3 to  Array.length Sys.argv - 1 do
			input_arr := !input_arr @ [int_of_string Sys.argv.(i)]
		done);
		run inch !trace !interval !eval !input_arr;
		close_in inch;
		exit 0
	end