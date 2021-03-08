let (<.>) g f = fun x -> g (f x)

module type MONAD = sig
  type 'a t

  val lift   : ('a -> 'b) -> ('a t -> 'b t)

  val return : 'a -> 'a t

  val join   : 'a t t -> 'a t

  val bind   : ('a -> 'b t) -> ('a t -> 'b t)

end

module Writer : MONAD = struct
  type 'a t = 'a * string

  let lift f = fun (x, s) -> (f x, s)

  let return x = (x, "")

  let join ((x, s1), s2) = (x, s1 ^ s2)

  let bind (f: 'a -> 'b * string) = join <.> lift f

end