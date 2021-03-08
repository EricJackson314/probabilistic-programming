type valu =
  | Frgt of comp
  | Summ of valu list
  | Unit
  | Pair of valu * valu

and comp =
  | Free of valu
  | Prod of comp list
  | Abst of valu * comp

and typ =
  | Valu of valu
  | Comp of comp


type exp =
  | Var of string
  | Fun of string * exp
  | App of exp * exp
  | Let of string * exp * exp 
  | To of exp * string * exp
  | Produce of exp
  | Thunk of exp
  | Force of exp