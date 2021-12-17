
This is the README file for SMA_star, an OCaml implementation of the SMA* search algorithm.

WARNING: The code is still very buggy. So far, the sliding puzzle only works on states that
are within a few moves of the solution.

The code mostly follows the design described in Artificial Intelligence: A Modern Approach, 
by Stuart Russell and Peter Norvig. The most important difference is that, instead of 
deleting high-cost nodes that don't fit in the queue, this implementation retains them as 
stubs that contain the node's action and f-cost. This makes regenerating nodes MUCH easier
than if they were literally deleted, and it also enables the algorithm to regenerate the most 
promising nodes first. Ineligible nodes, i.e. those with "infinite" cost, are deleted completely; 
they can't be regenerated unless the parent node is deleted and regenerated.


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
				taking the action a with state s as the starting point.

	make_action_generator: state -> action Generator.t

				a function that generates a thunk to produce actions from a state

				See Generator below. You'll need to provide SMA_star with a way
				to produce all of the actions that are allowed from a given state.

	is_goal: state -> bool

				is_goal s determines whether s is an acceptable goal state.

	delta_g_cost_of_action: state -> action -> int

				delta_g_cost_of_action s a returns the cost (i.e. the incremental increase 
				in the g-cost) incurred by applying action a to state s.

	h_cost_to_goal: state -> int

				a permissible heuristic cost function for the problem

	string_of_action: action -> string
	string_of_state:  state  -> string

				debugging functions


Generator		a module for creating thunk generators

			For each state encountered in a search, SMA_star requires a thunk 
			that returns an allowable action from that state each time it's called. 
			The function My_problem.make_action_generator takes the state and returns the thunk,
			and each function in this module converts some other kind of user- 
			supplied generator (or related function) into a thunk generator.

			These functions are actually polymorphic, but, for the sake of concreteness,
			the type variables have been replaced with state and action as defined in the user 
			module. For the real signatures, replace state with 'a and action with 'b.

	action t = unit -> action option

				the type of action generators, i.e. thunks that return actions

				An action generator returns (Some a) as long as there's 
				at least one action (a) that hasn't been generated yet, 
				and then returns None.

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

	beats x y	beats takes two elements and returns true if and only if the first one
			belongs higher in the queue than the second on.

	getloc x	Getter and setter for an element's integer location in the queue.	
	setloc x n	This is for rearranging the queue after the element's f-cost is increased.

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
	the type of queues used by SMA_star

module DEPQ: Typeof_Queue
	a module for creating array-based double-ended priority queues.

module type Typeof_Search =
    sig
      val search:
        queue_size:int ->
        ?max_depth:int ->
        Prob.state -> (Prob.action * Prob.state) list option
    end
	My_search.search ~queue_size initial_state 
	creates a DEP queue and uses the SMA* algorithm to find a goal state.
	The return value is an option containing a path of actions and states
	that leads from the initial state to the goal state.

module Make_with_queue:
             functor (Q: Typeof_Queue)
                     (Prob: Typeof_Problem) -> Typeof_Search
module Make: functor (Prob: Typeof_Problem) -> Typeof_Search

	Make (My_problem) creates a module that contains the search function.

	Make_with_queue (My_queue) (My_problem) accepts a user-supplied queue module
	in addition to the problem. The queue is expected to inform nodes 
	whenever their locations change and allow repositioning of any parent node 
	whose f-cost increases (when the known minimum f-cost of its successors increases). 


