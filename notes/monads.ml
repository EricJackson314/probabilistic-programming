let (<.>) g f = fun x -> g (f (x))
let flip f x y = f y x

module type MONAD = sig
  type 'a t
  
  val lift   : ('a -> 'b) -> ('a t -> 'b t)

  val return : 'a -> 'a t

  val join   : 'a t t -> 'a t

  val bind   : 'a t -> ('a -> 'b t) -> 'b t
end

module Monad_Utils (M : MONAD) = struct
  open M 

  let (=<<) f = join <.> lift f

  let (>>=) x f = f =<< x
  
  let (>=>) f g = fun x -> g =<< f x 
  
  let (<=<) f g = g >=> f

  let sequence (lst : 'a M.t list) =
    let f x lst = x >>= fun x' -> lst >>= fun lst' -> return (x'::lst') in
    List.fold_right f lst (return [])

  let mapM f = sequence <.> List.map f

  let forM x f = (mapM f) x

end

module Writer : MONAD = struct
  type 'a t = 'a * string

  let lift f = fun (x, s) -> f x, s

  let return x = x, ""
  
  let join ((x, s1), s2) = x, s1 ^ s2
  
  let bind v f = v |> (join <.> lift f)  
end

module Writer_Utils = Monad_Utils(Writer)
