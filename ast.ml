type var = string (* variable - it's a string *)
type pc = int (* pc - program counter *)
type numb = int

(**
 * In all instruction first 'pc' - current program counter, second 'pc' - in what point we should go
 *)
type expr = 
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MUL of expr * expr
  | DIV of expr * expr
  | MODULO of expr * expr
  | EQ of expr * expr
  | NE of expr * expr
  | GT of expr * expr
  | GE of expr * expr
  | LT of expr * expr
  | LE of expr * expr
  | AND of expr * expr
  | OR of expr * expr
  | NUMBER of numb
  | VAR of var

type instruction = 
  | ASSIGN of var * expr * pc 
  | WRITE of expr * pc     
  | READ of var * pc
  | WHILE of expr * pc * pc
  | IF of expr * pc * pc

let rec string_of_expr expression = match expression with
  | NUMBER num -> string_of_int num
  | VAR var -> var
  | PLUS (expr1, expr2) -> string_of_expr expr1 ^ " + " ^ string_of_expr expr2
  | MINUS (expr1, expr2) -> string_of_expr expr1 ^ " - " ^ string_of_expr expr2
  | MUL (expr1, expr2) -> string_of_expr expr1 ^ " * " ^ string_of_expr expr2
  | DIV (expr1, expr2) -> string_of_expr expr1 ^ " / " ^ string_of_expr expr2
  | MODULO (expr1, expr2) -> string_of_expr expr1 ^ " % " ^ string_of_expr expr2
  | EQ (expr1, expr2) -> string_of_expr expr1 ^ " == " ^ string_of_expr expr2
  | NE (expr1, expr2) -> string_of_expr expr1 ^ " != " ^ string_of_expr expr2
  | GT (expr1, expr2) -> string_of_expr expr1 ^ " > " ^ string_of_expr expr2
  | GE (expr1, expr2) -> string_of_expr expr1 ^ " >= " ^ string_of_expr expr2
  | LT (expr1, expr2) -> string_of_expr expr1 ^ " < " ^ string_of_expr expr2
  | LE (expr1, expr2) -> string_of_expr expr1 ^ " <= " ^ string_of_expr expr2
  | AND (expr1, expr2) -> string_of_expr expr1 ^ " && " ^ string_of_expr expr2
  | OR (expr1, expr2) -> string_of_expr expr1 ^ " || " ^ string_of_expr expr2;;

let string_of_instruction instruction = match instruction with
  | ASSIGN (var, expr, pc) -> (string_of_int pc) ^ ". " ^ var ^ " := " ^ (string_of_expr expr)
  | WRITE (expr, pc) -> (string_of_int pc) ^ ". write " ^ (string_of_expr expr)
  | READ (var, pc) -> (string_of_int pc) ^ ". read " ^ var
  | WHILE (expr, pc, pc') -> (string_of_int pc) ^ ". while (" ^ (string_of_expr expr) ^ ")"
  | IF (expr, pc, pc') -> (string_of_int pc) ^ ". if (" ^ (string_of_expr expr) ^ ")";;

let rec print_expressions list_of_expr = match list_of_expr with
  | [] -> 
  | head :: other_list -> print_string (string_of_expr head); print_string "\n"; print_expressions other_list;;

(**
 *  Example of using: 
 *  1) print_string (string_of_expr (PLUS ( NUMBER 10, NUMBER 20 )));;
 * 
 *  2) let expr1 = PLUS (NUMBER 10, NUMBER 20);;
 *     let expr2 = MUL (NUMBER 30, expr1);;
 *     let expr3 = EQ (expr2, NUMBER 10);;
 *
 *     print_string (string_of_expr (expr3));; 
 *)

(*
let var_to_string v = match v with
  | X -> "x" | Y -> "y" | Z -> "z"

let inst_to_string i = match i with
  | INC v -> "inc " ^ (var_to_string v)
  | DEC v -> "dec " ^ (var_to_string v)
  | ZERO (v,pc,pc') -> "zero " ^ (var_to_string v) ^ " " ^ 
      (string_of_int pc) ^ " else " ^ (string_of_int pc')
  | STOP -> "stop"

let prettyprint is = 
  let rec printlist line is = match is with
    | [] -> print_newline ()
    | i::is' ->
	Printf.printf "%3i: %s" line (inst_to_string i);
	print_newline ();
	printlist (line+1) is' in
    printlist 1 is
*)