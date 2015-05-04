open Ast
open Parser
open Lexer

let run chn trace interval = 
	let lexbuf = Lexing.from_channel chn in
  	let prog = Parser.program Lexer.token lexbuf in
  	if trace then
  		Ast.print_instructions prog
  	else if interval then 
  	begin
  		(* run interval analysis *)
  	end
  	else 	 
  		Eval.run prog
  
let _ =
    let trace = ref false in
	let interval = ref false in	
    let filenum  = ref 0 in
    let speclist = [
		    ("-trace", Arg.Set trace, "Trace instr. list, default is false");
		    ("-interval", Arg.Set interval, "Interval analysis execution, default is false")] in               
    let usagestr = "Usage: " ^ Sys.argv.(0) ^ " [options] <filename>" in
	if not (!Sys.interactive) then
      begin
      	Arg.parse speclist
	  	(fun s -> try
  		      let inch = open_in s in
		      incr filenum;
		      print_string ("Opening file \"" ^ s ^ "\"\n\n");
		      run inch !trace !interval;
		      close_in inch;
		      exit 0
  	            with Sys_error e -> raise (Arg.Bad e)
	  	) usagestr;
		(if !filenum != 1 then Arg.usage speclist usagestr else ())
      end
    else ()


  
