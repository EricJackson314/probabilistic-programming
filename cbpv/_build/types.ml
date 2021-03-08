open Ast

exception TypeError of string

let rec type_check (g : (string * typ) list) : exp -> typ = function
  | Var s -> type_var g s
  | Fun (x, e) -> type_fun g x e
  | App (e1, e2) -> type_app g e1 e2
  | Produce e -> type_produce g e
  | Thunk e -> type_thunk g e
  | Force e -> type_force g e
  | _ -> failwith "todo"

and type_var g s =
  match List.find_opt (fun (x, t) -> x = s) g with
  | Some (x, t) -> t
  | None -> raise (TypeError "Variable Type Error.")

and type_fun g x e =
  match type_check ((x, Valu Unit)::g) e with
    | Comp t2 -> Comp (Abst (Unit, t2))
    | _ -> raise (TypeError "Function Type Error")

and type_app g e1 e2 =
  match type_check g e1 with
      | Valu t1 ->
        begin
          match type_check g e2 with
            | Comp (Abst (t2, t3)) when t1 = t2 -> Comp t3
            | _ -> raise (TypeError "Application Type Error.")
        end
      | _ -> raise (TypeError "Application Type Error.")

and type_produce g e =
  match type_check g e with
  | Valu t -> Comp (Free t)
  | _ -> raise (TypeError "Produce Type Error.")

and type_thunk g e =
  match type_check g e with
  | Comp t -> Valu (Frgt t)
  | _ -> raise (TypeError "Thunk Type Error.")

and type_force g e =
  match type_check g e with
  | Valu (Frgt t) -> Comp t
  | _ -> raise (TypeError "Force Type Error.")

and check e = type_check [] e