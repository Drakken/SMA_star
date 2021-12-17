(*
 * SMA* search algorithm
 * from S. Russell & P. Norvig, Artificial Intelligence: A Modern Approach
 * copyright (c) 2021 Daniel S. Bensen
 *)

module type T = sig

  type t
  val beats: t -> t -> bool

  val setloc: t -> int -> unit
  val getloc: t -> int

  val print_row: t list -> unit

end
