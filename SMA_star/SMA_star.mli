

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

module type Typeof_Make =
  functor (Prob : Typeof_Problem)
          (Queue: Typeof_Queue) ->
    sig
      val search:
        queue_size:int ->
        ?max_depth:int -> Prob.state -> (Prob.action * Prob.state) list option
    end

module Make :
  functor (Prob : Typeof_Problem)
          (Queue: Typeof_Queue) ->
    sig
      val search:
        queue_size:int ->
        ?max_depth:int -> Prob.state -> (Prob.action * Prob.state) list option
    end
