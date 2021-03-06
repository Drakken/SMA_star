(*
 * SMA* search algorithm
 * from S. Russell & P. Norvig, Artificial Intelligence: A Modern Approach
 * copyright (c) 2021 Daniel S. Bensen
 *)

(*
module Element: sig include module type of struct include Element end end
module Generator: module type of Generator
*)

module Element: sig
module type T =
  sig
    type t
    val beats : t -> t -> bool
    val setloc : t -> int -> unit
    val getloc : t -> int
    val to_strings : t -> string list
  end
end

module Generator: sig
type 'a t = unit -> 'a option
val of_stream_function : (int -> 'a option) -> unit -> 'a option
val of_parser_maker : ('a -> int -> 'b option) -> 'a -> unit -> 'b option
val of_array_maker : ('a -> 'b array) -> 'a -> unit -> 'b option
val of_sequence_function : ('a -> ('b * 'a) option) * 'a -> unit -> 'b option
val of_stepper_and_value_maker :
  ('a -> ('b -> ('c * 'b) option) * 'b) -> 'a -> unit -> 'c option
val of_list_maker : ('a -> 'b list) -> 'a -> unit -> 'b option
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
  val strings_of_state:  state  -> string list

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
          val  top  : t -> element
          val  pop  : t -> element
          val drop  : t -> element 
          val bottom: t -> element
          val is_full: t -> bool
          val update: t -> int -> unit
          val element_at: t -> int -> element
          val print: t -> unit
        end
end

module DEPQ: Typeof_Queue


module type Typeof_Make =
  functor (Prob: Typeof_Problem) ->
    sig
      val search:
        queue_size: int ->
        ?max_depth: int ->
        ?printing: bool ->
        Prob.state -> (Prob.action * Prob.state) list option
    end

module Make: Typeof_Make 

module Make_with_queue: functor (Q: Typeof_Queue) -> Typeof_Make 




