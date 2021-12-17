(*
 * SMA* search algorithm
 * from S. Russell & P. Norvig, Artificial Intelligence: A Modern Approach
 * copyright (c) 2021 Daniel S. Bensen
 *)


module Utils: sig
  exception Not_found_in of string
  val not_found_in : string -> 'a
  val fail_if : bool -> string -> unit
  val verify : bool -> string -> unit
  val try_or_die : ('a -> 'b) -> 'a -> string -> 'b
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
  val ( >>=! ) : 'a option -> ('a -> unit) -> unit
  val from_some : 'a option -> 'a
  val make_counter : int -> unit -> int
  val fold_times : ('a -> 'a) -> 'a -> int -> 'a
  val is_even : int -> bool
  val is_odd : int -> bool
  val demand_int : string -> int
  val ( ** ) : int -> int -> int
  module A: sig
    val swap_cells : 'a array -> int -> int -> unit
  end
  module L :
  sig
    val remove_if : ('a -> bool) -> 'a list-> 'a list
    val remove_object : 'a -> 'a list -> 'a list
    val min_of : ('a -> 'b) -> 'a list -> 'b
    val sum : int list -> int
    val combine : ('a -> 'a -> 'a) -> 'a list -> 'a
    val interleave : ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a
    val transpose : 'a list list -> 'a list list
    val snip : int -> 'a list -> 'a list * 'a list
    val print_strings : string list -> unit
    val print_lines : ('a -> string list) -> 'a list -> unit
  end
end


module Element: sig
  module type T = sig
    type t
    val beats: t -> t -> bool
    val setloc: t -> int -> unit
    val getloc: t -> int
    val print_row: t list -> unit
  end
end


module Generator:
  sig
    type 'a t = unit -> 'a option

    val of_parser_maker: ('a -> int -> 'b option) -> 'a -> unit -> 'b option

    val of_stepper_and_value_maker: ('a -> ('b -> ('c * 'b) option) * 'b) -> 'a -> unit -> 'c option

    val of_array_maker: ('a -> 'b array) -> 'a -> unit -> 'b option

    val of_list_maker: ('a -> 'b list) -> 'a -> unit -> 'b option
  end


module type Typeof_Problem = sig

  type state

  type action

  val trivial_action: action

  val next_state: state -> action -> state

  val make_action_generator: state -> action Generator.t

  val is_goal: state -> bool

  val delta_g_cost_of_action: state -> action -> int

  val h_cost_to_goal: state -> int

  val string_of_action: action -> string
  val string_of_state:  state  -> string

end

module type Typeof_Queue = sig
    module Make:
      functor (E: Element.T) ->
        sig
          type element = E.t
          type t
          val make : int -> element -> t
          val size : t -> int
          val insert: t -> element -> unit
          val otop  : t -> element option
          val opop  : t -> element option
          val odrop : t -> element option
          val obottom: t -> element option
          val is_full: t -> bool
          val update: t -> int -> unit
          val element_of_loc: t -> int -> element
        end
end

module DEPQ: Typeof_Queue


module type Typeof_Make =
  functor (Prob: Typeof_Problem) ->
    sig
      val search:
        queue_size:int ->
        ?max_depth:int ->
        Prob.state ->
        (Prob.action * Prob.state) list option
    end

module Make: Typeof_Make 

module Make_with_queue: functor (Q: Typeof_Queue) -> Typeof_Make 






