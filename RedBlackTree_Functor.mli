(*
 * a functional implementation of red-black trees
 * based on Cormen et. al., Introduction To Algorithms
 * copyright 2021 Daniel S. Bensen
 *)

module type Element_Type =
  sig
    type t
    type k
    type v
    val key: t -> k
    val value: t -> v
    val compare: k -> k -> int
  end

module type Make_RedBlackTree_Type =
  functor (E: Element_Type) ->
    sig
      type t
      type element = E.t
      val empty: t
      val size: t -> int
      val mem: E.k -> t -> bool
      val find: E.k -> t -> element option
      val fold: ('a -> element -> 'a) -> 'a -> t -> 'a
      val map :        (element -> element) -> t -> t
      val mapi: (int -> element -> element) -> t -> t
      val iter:         (element -> 'a) -> t -> unit
      val iteri: (int -> element -> 'a) -> t -> unit
      val insert: t -> element -> t
      val remove:       t -> E.k -> t
      val remove_if_in: t -> E.k -> t
    end
