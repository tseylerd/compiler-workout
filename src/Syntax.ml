(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
open List
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let asboolean s = s <> 0

    let asinteger b = if b then 1 else 0

    let rec eval s e = match e with
      | Const c -> c
      | Var v -> s v
      | Binop (o, el, er) ->
        let lr = eval s el in
        let rr = eval s er in
        match o with
          | "!!" -> asinteger (asboolean lr || asboolean rr)
          | "&&" -> asinteger (asboolean lr && asboolean rr)
          | "==" -> asinteger (lr = rr) 
          | "!=" -> asinteger (lr <> rr) 
          | "<=" -> asinteger (lr <= rr) 
          | ">=" -> asinteger (lr >= rr)
          | "<" -> asinteger (lr < rr)
          | ">" -> asinteger (lr > rr)
          | "+" -> lr + rr 
          | "-" -> lr - rr
          | "*" -> lr * rr 
          | "/" -> lr / rr 
          | "%" -> lr mod rr     
          | _ -> failwith "unsupported binary operator"

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (state, inlist, outlist) statement = match statement with
      | Read x -> 
        let head = hd inlist in 
        let tail = tl inlist in 
        (Expr.update x head state, tail, outlist)
      | Write x -> 
        let result = Expr.eval state x in 
        (state, inlist, outlist @ [result])
      | Assign (s, e) -> 
        let result = Expr.eval state e in 
        (Expr.update s result state, inlist, outlist)
      | Seq (first, second) -> (eval (eval (state, inlist, outlist) first) second)
                                                         
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
