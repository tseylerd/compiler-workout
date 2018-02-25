open GT   
open List    
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval (stack, (state, inlist, outlist)) program = 
  match program with
    | [] -> (stack, (state, inlist, outlist))
    | command :: programtail -> 
      match command with
        | BINOP s ->
          let secondOperand = hd stack in
          let firstOperand = hd (tl stack) in
          let result = Syntax.Expr.eval state (Syntax.Expr.Binop(s, Syntax.Expr.Const firstOperand, Syntax.Expr.Const secondOperand)) in
          eval (result :: (tl (tl stack)), (state, inlist, outlist)) programtail 
        | CONST i -> eval (i :: stack, (state, inlist, outlist)) programtail
        | READ -> eval (hd inlist :: stack, (state, tl inlist, outlist)) programtail
        | WRITE -> eval (tl stack, (state, inlist, outlist @ [hd stack])) programtail
        | LD s -> eval (state s :: stack, (state, inlist, outlist)) programtail
        | ST s -> eval (tl stack, (Syntax.Expr.update s (hd stack) state, inlist, outlist)) programtail

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec flatten expr =
	match expr with
	  | Syntax.Expr.Const i -> [CONST i]
	  | Syntax.Expr.Var s -> [LD s]
	  | Syntax.Expr.Binop (o, l, r) -> flatten l @ flatten r @ [BINOP o]

let rec compile statement =
	match statement with
	  | Syntax.Stmt.Read s -> [READ] @ [ST s]
	  | Syntax.Stmt.Write e -> flatten e @ [WRITE]
	  | Syntax.Stmt.Assign (s, e) -> flatten e @ [ST s]
	  | Syntax.Stmt.Seq (l, r) -> compile l @ compile r



