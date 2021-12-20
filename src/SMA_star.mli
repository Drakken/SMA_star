(*
 * SMA* search algorithm
 * from S. Russell & P. Norvig, Artificial Intelligence: A Modern Approach
 * copyright (c) 2021 Daniel S. Bensen
 *)


module Element:   module type of Element
module Generator: module type of Generator


module type Typeof_Problem = sig

  type state

  type action

  val trivial_action: action

  val next_state: state -> action -> state

  val make_action_generator: state -> action Generator.t

  val is_goal: state -> bool

  val delta_g_cost_of_action: state -> action -> int

  val h_cost_to_goal: state -> int

  val strings_of_action: action -> string
  val strings_of_state:  state  -> string

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
          val print: t -> unit
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






