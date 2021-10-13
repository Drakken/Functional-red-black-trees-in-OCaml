(*
 * a functional implementation of red-black trees
 * based on Cormen et. al., Introduction To Algorithms
 * copyright (c) 2021 Daniel S. Bensen
 *)

type color = Red | Black

type 'a t = Empty | Node of 'a t * 'a * 'a t * color

val empty: 'a t

val size:         'a t -> int
val black_height: 'a t -> int

val is_member: 'a -> 'a t -> bool

val of_list: 'a list -> 'a t
val to_list: 'a t -> 'a list

val insert:     'a t -> 'a -> 'a t
val insert_new: 'a t -> 'a -> 'a t

val remove:     'a t -> 'a -> 'a t
val remove_all: 'a t -> 'a -> 'a t

val merge: 'a t -> 'a t -> 'a t
val union: 'a t -> 'a t -> 'a t

val fold_left:  ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val map:         ('a -> 'b) -> 'a t -> 'b t
val mapi: (int -> 'a -> 'b) -> 'a t -> 'b t

val iter:         ('a -> unit) -> 'a t -> unit
val iteri: (int -> 'a -> unit) -> 'a t -> unit

module type Typeof_Element =
  sig
    type t
    type tkey
    type tval
    val key: t -> tkey
    val value: t -> tval
    val compare: tkey -> tkey -> int
  end

module type Typeof_Make =
  functor (E: Typeof_Element) ->
    sig
      type element = E.t
      type nonrec t = element t
      val empty: t
      val size: t -> int
      val is_member: E.tkey -> t -> bool

      val find:  E.tkey -> t -> element option
      val value: E.tkey -> t -> E.tval  option

      val of_list: element list -> t
      val to_list: t -> element list

      val insert:     t -> element -> t
      val insert_new: t -> element -> t

      val remove:     t -> E.tkey -> t
      val remove_all: t -> E.tkey -> t

      val merge: t -> t -> t
      val union: t -> t -> t

      val fold_left:  ('a -> element -> 'a) -> 'a -> t -> 'a
      val fold_right: ('a -> element -> 'a) -> 'a -> t -> 'a

      val iter:         (element -> unit) -> t -> unit
      val iteri: (int -> element -> unit) -> t -> unit

    end

module Make: Typeof_Make
