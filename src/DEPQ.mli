(*
 * array-based double-ended priority queue (DEP queue)
 * heap implementation based on Cormen et. al., Introduction to Algorithms
 * copyright (c) 2021 Daniel S. Bensen
 *)

exception Overflow
exception Underflow


module Make (E: Element.T): sig

  type element = E.t
  type t

  val make : int -> element -> t

  val max:   t -> int
  val size : t -> int

  val  insert: t -> element -> unit
  val oinsert: t -> element -> unit option

  val top  : t -> element
  val pop  : t -> element
  val drop : t -> element
  val bottom:t -> element

  val otop  : t -> element option
  val opop  : t -> element option
  val odrop : t -> element option
  val obottom:t -> element option

  val is_empty: t -> bool
  val is_full:  t -> bool

  val clear: t -> unit

  val update: t -> int -> unit

  val element_at: t -> int -> element

  val print: t -> unit

end
