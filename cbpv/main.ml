open Ast
open Eval
open Types

let (<.>) g f x = g (f x)

let parse (s : string) : exp =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let interp = (fun e -> (check e, eval e)) <.> parse