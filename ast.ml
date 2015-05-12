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
  | SKIP of pc
  | ASSIGN of pc * var * expr
  | WRITE of pc * expr
  | READ of pc * var 
  | WHILE of pc * expr * pc (* The second pc - pc after '}' *)
  | IF of pc * expr * pc
  | FUNC of pc * var * expr list
  | RIGHTBRACKET of pc * pc
  
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

let rec string_of_expr_list expr_list = match expr_list with
  | last_elem :: [] -> string_of_expr last_elem
  | head :: tail -> string_of_expr head ^ ", " ^ string_of_expr_list tail
  | [] -> ""

let string_of_instruction instruction = match instruction with
  | SKIP (pc) -> (string_of_int pc) ^ ". skip"
  | ASSIGN (pc, var, expr) -> (string_of_int pc) ^ ". " ^ var ^ " := " ^ (string_of_expr expr)
  | WRITE (pc, expr) -> (string_of_int pc) ^ ". write " ^ (string_of_expr expr)
  | READ (pc, var) -> (string_of_int pc) ^ ". read " ^ var
  | WHILE (pc, expr, pc') -> (string_of_int pc) ^ ". while (" ^ (string_of_expr expr) ^ ")"
  | IF (pc, expr, pc') -> (string_of_int pc) ^ ". if (" ^ (string_of_expr expr) ^ ")"
  | FUNC (pc, var, expr_list) -> (string_of_int pc) ^ ". " ^ var ^ "(" ^ string_of_expr_list expr_list ^ ")"
  | RIGHTBRACKET (pc, pc') -> (string_of_int pc) ^ ". }";;

let print_expression expr = print_string (string_of_expr expr);;

let rec print_expressions list_of_expr = match list_of_expr with
  | [] -> print_string ""
  | head :: other_list -> print_expression head; print_string "\n"; print_expressions other_list;;

let rec print_instructions list_of_inst = match list_of_inst with
  | [] -> print_string ""
  | head :: other_list -> print_string (string_of_instruction head); print_newline (); print_instructions other_list;;

let get_pc instr = match instr with
  | SKIP (pc) -> pc
  | ASSIGN (pc, var, expr) -> pc
  | WRITE (pc, expr) -> pc
  | READ (pc, var) -> pc
  | WHILE (pc, expr, pc') -> pc
  | IF (pc, expr, pc') -> pc
  | FUNC (pc, var, var_list) -> pc
  | RIGHTBRACKET (pc, pc') -> pc

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