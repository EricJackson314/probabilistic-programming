(* Helpers *)
let id x = x
let const x _  = x
let flip f x y = f y x
let compose f g x = x |> g |> f
let (<.>) f g = fun x -> x |> g |> f
let (>.>) g f = fun x -> x |> g |> f
let cons x xs = x :: xs

(* Monoids *)
module type MONOID = sig
  type t
  val mempty  : t
  val mappend : t -> t -> t
end

module Monoid_Utils (M : MONOID) = struct
  open M
  let (<+>) x y = mappend x y
  let concat xs = List.fold_left (<+>) mempty xs
end

module Sum : (MONOID with type t = int) = struct
  type t = int
  let mempty = 0
  let mappend = (+)
end

module MSum = Monoid_Utils(Sum)

module Product : (MONOID with type t = int) = struct
  type t = int
  let mempty = 1
  let mappend = ( * )
end

module MProduct = Monoid_Utils(Product)

module StringM : (MONOID with type t = string) = struct
  type t = string
  let mempty = ""
  let mappend = (^)
end

module MString = Monoid_Utils(Product)

module All : (MONOID with type t = bool) = struct
  type t = bool
  let mempty = true
  let mappend = (&&)
end

module MAll = Monoid_Utils(All)

module Any : (MONOID with type t = bool) = struct
  type t = bool
  let mempty = false
  let mappend = (||)
end

module MAny = Monoid_Utils(Any)


(* Functors *)
module type FUNCTOR = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module Functor_Utils(F : FUNCTOR) = struct
  open F
  let (<$>) = fmap
  let (<$) r x = fmap (const r x)
  let ($>) r x = flip (<$) r x
  let void f x = fmap (fun x -> ignore (f x)) x
end

module ListF : (FUNCTOR with type 'a t = 'a list) = struct
  type 'a t = 'a list
  let fmap = List.map
end

module FList = Functor_Utils(ListF)

module OptionF : (FUNCTOR with type 'a t = 'a option) = struct
  type 'a t = 'a option
  let fmap f = function
    | Some x -> Some (f x)
    | None   -> None 
end

module FOption = Functor_Utils(OptionF)

(* Applicative *)
module type APPLICATIVE = sig
  type 'a t
  include FUNCTOR with type 'a t := 'a t
  val pure : 'a -> 'a t
  val ap   : ('a -> 'b) t -> 'a t -> 'b t
end

module Applicative_Utils (A : APPLICATIVE) = struct
  open A
  module FunU = Functor_Utils(A)
  include FunU

  let (<*>) = ap

  let liftA f x = f <$> x
  let liftA2 f x y = f <$> x <*> y
  let liftA3 f x y z = f <$> x <*> y <*> z

  let ( <* ) r x = const <$> r <*> x
  let ( *> ) r x = (fun _ y -> y) <$> r <*> x

  let rec sequenceA = function
  | []      -> pure []
  | x :: xs -> List.cons <$> x <*> sequenceA xs
  let sequenceA_ xs = List.fold_right ( *> ) xs (pure ())
  let traverseA f = (List.map f) >.> sequenceA

end