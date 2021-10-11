(*
 * a functional implementation of red-black trees
 * based on Cormen et. al., Introduction To Algorithms
 * copyright 2021 Daniel S. Bensen
 *)

type 'a t

val empty: 'a t

val size: 'a t -> int

val is_member: 'a -> 'a t -> bool

val of_list: 'a list -> 'a t
val to_list: 'a t -> 'a list

val insert:        'a t -> 'a -> 'a t
val insert_unique: 'a t -> 'a -> 'a t

val remove:     'a t -> 'a -> 'a t
val remove_all: 'a t -> 'a -> 'a t

val union:        'a t -> 'a t -> 'a t
val union_unique: 'a t -> 'a t -> 'a t

val fold: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val map:         ('a -> 'b) -> 'a t -> 'b t
val mapi: (int -> 'a -> 'b) -> 'a t -> 'b t

val iter:         ('a -> unit) -> 'a t -> unit
val iteri: (int -> 'a -> unit) -> 'a t -> unit

module type Element_Type =
  sig
    type t
    type k
    type v
    val key: t -> k
    val value: t -> v
    val compare: k -> k -> int
  end

module type Typeof_Make =
  functor (E: Element_Type) ->
    sig
      type element = E.t
      type nonrec t = element t
      val empty: t
      val size: t -> int
      val is_member: E.k -> t -> bool

      val find:  E.k -> t -> element option
      val value: E.k -> t -> E.v option

      val of_list: element list -> t
      val to_list: t -> element list

      val insert:        t -> element -> t
      val insert_unique: t -> element -> t

      val remove:     t -> E.k -> t
      val remove_all: t -> E.k -> t

      val union:        t -> t -> t
      val union_unique: t -> t -> t

      val fold: ('a -> element -> 'a) -> 'a -> t -> 'a

      val iter:         (element -> unit) -> t -> unit
      val iteri: (int -> element -> unit) -> t -> unit

    end
