(*
 * a functional implementation of red-black trees
 * based on Cormen et. al., Introduction To Algorithms
 * copyright 2021 Daniel S. Bensen
 *)

type 'a t = Empty | Node of 'a t * 'a * 'a t * color

val empty: 'a t

val mem: 'a -> 'a t -> bool

val size: 'a t -> int

val insert: 'a t -> 'a -> 'a t

val remove: 'a t -> 'a -> 'a t

val remove_if_in: 'a t -> 'a -> 'a t

val fold: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val map: ('a -> 'b) -> 'a t -> 'b t

val mapi: (int -> 'a -> 'b) -> 'a t -> 'b t

val iter ('a -> ()) -> 'a t -> ()

val iteri (int -> 'a -> ()) -> 'a t -> ()

