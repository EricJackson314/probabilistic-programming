
(* The type of tokens. *)

type token = 
  | TO
  | THUNK
  | RPAREN
  | QUOTE
  | PRODUCE
  | LPAREN
  | LET
  | LAMBDA
  | ID of (string)
  | FORCE
  | EOF
  | DOT
  | BE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp)
