module type MONOID = sig
  type 'a t
  val mempty  : 'a t
  val mappend : 'a t -> 'a t -> 'a t
end

module type FUNCTOR = sig
  type 'a t 
  val fmap : ('a -> 'b) -> ('a t -> 'b t)
end

module type MONAD = sig
  (** ENDOFUNCTOR *************************************************************)
  type 'a t
  val fmap   : ('a -> 'b) -> ('a t -> 'b t) (* also called lift *)
  (** NATURAL TRANSFORMATIONS *************************************************)
  val return : 'a -> 'a t (* identity *)
  val join   : ('a t) t -> 'a t (* composition *)
  (* [bind v f = join ((fmap f) v)] *)
  val bind   : 'a t -> ('a -> 'b t)  -> 'b t
end

module Maybe : MONAD = struct
  type 'a t = None | Some of 'a
  let fmap f = function
    | None -> None
    | Some x -> Some (f x)  
  let return x = Some x
  let join = function
  | None -> None
  | Some x -> match x with None -> None | y -> y
  let bind v f = join ((fmap f) v)

  let bind' v f = v |> fmap f |> join  


  let comp (f: 'a -> 'b) (g : 'b -> 'c) = fun (x : 'a) -> g (f x) 
  let test : ('a -> 'b) -> 'a t t -> 'b t t =  comp fmap fmap
  let test' : (('a t -> 'b t) -> 'c) -> ('a -> 'b) -> 'c = comp fmap

  let test'' f x = join (test f x)

  let test''' = test (fun x -> x + 1)


end

module Writer : MONAD = struct
  type 'a t = 'a * string
  let fmap f = fun (x, s) -> f x, s
  let return x = x, ""
  let join = fun ((x, s1), s2) -> x, s1 ^ s2
  let bind v f = join ((fmap f) v) 
end

let comp (f: 'a -> 'b) (g : 'b -> 'c) = fun x -> g (f x) 