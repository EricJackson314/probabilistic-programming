open Ast

exception EvaluationError of string

let rec substitute (s : string) (z : exp) : exp -> exp = function
| Var x when x = s           -> z
| Var x  as e                -> e
| Fun (x, e) when x = s      -> Fun (x, e)
| Fun (x, e)                 -> Fun (x, substitute s z e) 
| App (e1, e2)               -> App (substitute s z e1, substitute s z e2)
| Let (x, e1, e2) when x = s -> Let (x, substitute s z e1, e2)
| Let (x, e1, e2)            -> Let (x, substitute s z e1, substitute s z e2)
| To (e1, x, e2) when x = s  -> To (substitute s z e1, x, e2)
| To (e1, x, e2)             -> To (substitute s z e1, x, e2)
| Produce e                  -> Produce (substitute s z e)
| Thunk e                    -> Thunk (substitute s z e)
| Force e                    -> Force (substitute s z e)

let rec eval : exp -> exp  = function
 | Var x           -> failwith "todo"
 | Fun (x, e)      -> Fun (x, e)
 | App (e1, e2)    -> eval_app e1 e2
 | Let (x, e1, e2) -> eval_let x e1 e2
 | To (e1, x, e2)  -> eval_to e1 x e2
 | Produce e       -> Produce e
 | Thunk e         -> Thunk e
 | Force e         -> eval_force e

and eval_app e1 e2 =
  match eval e2 with
    | Fun (x, e) -> eval (substitute x e1 e)
    | _          -> raise (EvaluationError "Application Error.") 

and eval_let x e1 e2 = eval (substitute x e1 e2)

and eval_to e1 x e2 =  
  match eval e1 with
  | Produce e' -> eval (substitute x e' e2)
  | _          -> raise (EvaluationError "To Error.")

and eval_force e = 
  match eval e with
    | Thunk e -> eval e
    | _       -> raise (EvaluationError "Force Error.")