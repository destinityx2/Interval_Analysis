open Ast
open Parser
open Lexer
open Printf

let run chn trace interval eval input_arr = 
	let lexbuf = Lexing.from_channel chn in
  	let prog = Parser.program Lexer.token lexbuf in
  	if trace then
  		Ast.print_instructions prog
  	else if interval then 
  	begin
  		(* run interval analysis *)
  	end
  	else if eval then
  		Eval.run prog input_arr
  
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


  
