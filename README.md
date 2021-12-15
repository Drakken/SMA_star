
This is the README file for SMA_star, an OCaml implementation of the SMA* search algorithm.

WARNING: As of December 2021, this project is still very much a work in progress. 
It's intended to be production quality at some point in time, but it's probably not there yet.

The code mostly follows the design described in Artificial Intelligence: A Modern Approach, 
by Stuart Russell and Peter Norvig. The most important difference is that, instead of 
deleting high-cost nodes that don't fit in the queue, this implementation retains them as 
stubs that contain the node's action and f-cost. This makes regenerating nodes MUCH easier
than if they were literally deleted, and it also enables the algorithm to regenerate the most 
promising nodes first. Ineligible nodes, i.e. those with "infinite" cost, are deleted completely; 
they can't be regenerated unless the parent node is deleted and regenerated.

SMA_star.Make can accept a user-supplied queue module, but the queue has to inform nodes 
whenever their locations change, because a parent node's f-cost increases when the minimum 
f-cost of its successors increases. The SMA_star package includes TestQ, a module for 
array-based DEP queues.


User Guide


Typeof_Problem		the type of user-supplied modules that define search applications

			A module of this type must completely describe the space of states 
			to be searched.

	state			the type of states that represent the results of full or partial 
				solutions to the problem (including the starting state)

	action			the type of information required to specify an action that leads from 
				a given state to one of its successors

	trivial_action		a simple, arbitrary value of type action. It nominally leads to the
				starting state, but it's never used, so you can give it any value you 
				want. Its purpose is to make the search code use slightly less memory.

	next_state: state -> action -> state

				next_state s a returns the state (i.e. the successor) that results from 
				taking the action described by a with state s as the starting point.

	make_action_generator: state -> action Generator.t

				a function that generates a thunk to produce actions from a state

				See Generator below. You'll need to provide SMA_star with a way
				to produce all of the actions that are allowed from a given state.

	is_goal: state -> bool

				is_goal s determines whether s is an acceptable goal state.

	dg_cost_of_action: state -> action -> int

				dg_cost_of_action s a returns the cost (i.e. the incremental increase 
				in the g-cost) required to take action a, starting from state s.

	h_cost_to_goal: state -> int

				a permissible heuristic cost function for the problem

	string_of_action: action -> string
	string_of_state:  state  -> string


Generator		a module for creating thunk generators

			For each state encountered in a search, SMA_star requires a thunk 
			that returns an allowable action from that state each time it's called. 
			make_action_generator takes the state and returns the thunk, and 
			each function in this module converts some other kind of user- 
			supplied generator (or related function) into a thunk generator.

			These functions are actually polymorphic, but, for the sake of simplicity,
			we'll pretend they only apply to states and actions as described in the user 
			module. For the real signatures, replace state with 'a and action with 'b.

	action t = unit -> action option

				the type of action generators, i.e. thunks that return actions

				An action generator returns (Some a) as long as there's 
				at least one action (a) that hasn't been generated yet, 
				and then returns None the next time it's called.

	of_list_maker:  (state -> action list)  -> state -> unit -> action option
	of_array_maker: (state -> action array) -> state -> unit -> action option

				If you have a function f that returns a list (or array) of actions,
				of_list_maker f (or of_array_maker f) will return a thunk generator.

	of_parser_maker: (state -> int -> action option) -> state -> unit -> action option

				The user-supplied function takes a state and returns a function 
				that can be passed to Stream.from to create an action stream.

	of_stepper_and_value_maker:
		(state -> ('c -> (action * 'c) option) * 'c) -> state -> unit -> action option

				The user-supplied function returns a function and an initial value
				that can be passed to Seq.unfold to create an action sequence.

	roll your own		You can think of a thunk generator as a generator of parsers that
				don't take integers, or a generator of stepping functions that 
				don't need explicit initial values.


Element.T		the type of modules that can be used in the queue
			If you provide your own queue module, it must contain
			a functor called Make that accepts modules of this type.

	t		the type of elements in the queue

	id x		id takes an element and returns its integer id number.
			You can use Utils.make_counter to create an id generator. 

	beats x y	beats takes two elements and returns true iff the first one
			belongs higher in the queue than the second on.

	getloc x	Getter and setter for an element's integer location in the queue.	
	setloc x n	This is for fixing the queue after the element's f-cost is updated.

  end


module type Typeof_Queue
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
        end
end

module type Typeof_Make =
  functor (Prob : Typeof_Problem)
          (Queue: Typeof_Queue) ->
    sig
      val search:
        queue_size:int ->
        ?max_depth:int -> Prob.state -> Prob.action list option
    end

module Make :
  functor (Prob : Typeof_Problem)
          (Queue: Typeof_Queue) ->
    sig
      val search:
        queue_size:int ->
        ?max_depth:int -> Prob.state -> Prob.action list option
    end


SMA_star.Make