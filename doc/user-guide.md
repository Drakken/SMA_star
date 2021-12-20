
# User Guide


 **Typeof_Problem**
the type of user-supplied modules that define search applications. A module of this type must completely describe the space of states  to be searched.

- **state**
the type of states that represent the results of full or partial solutions to the problem (including the initial state)

- **action**
the type of information required to specify an action that leads from a given state to one of its successors

- **trivial_action**
a simple, arbitrary value of type action. It nominally leads to the starting state, but it's never used, so you can give it any value you want. Its only purpose is to make the search code use slightly less memory.

- **next_state**: *state -> action -> state*
next_state s a returns the successor state that results from taking the action a with state s as the starting point.

- **make_action_generator**: *state -> action Generator.t*
a function that generates a thunk to produce actions from a state. See Generator below. You'll need to provide SMA_star with a way to produce all of the actions that are allowed from a given state.

- **is_goal**: *state -> bool*
`is_goal s` determines whether s is an acceptable goal state.

- **delta_g_cost_of_action**: *state -> action -> int*
`delta_g_cost_of_action s a` returns the cost (i.e. the increase in the g-cost) incurred by applying action a to state s.

- **h_cost_to_goal**: *state -> int*
a permissible heuristic cost function for the problem

- **strings_of_action**: *action -> string*
**strings_of_state**:  *state  -> string*
`strings_of_action a` and `strings_of_state s` should each return an ascii picture of its argument, represented as a list of character strings, one string per line. They're used to display the solution path on the command line.


**Generator**
a module for creating thunk generators. For each state encountered in a search, SMA_star requires a thunk that returns an allowable action from that state each time it's called. The function `My_problem.make_action_generator` takes the state and returns the supplied generator (or related function) into a thunk generator. 

These functions are actually polymorphic, but, for the sake of concreteness, the type variables have been replaced with `state` and `action` as defined in the user module. For the real signatures, replace `state` with `'a` and `action` with `'b`.

- **action t** = *unit -> action option*
the type of action generators, i.e. thunks that return actions. An action generator returns `Some a` as long as there's at least one action (a) that hasn't been generated yet, and then returns `None`.

- **of_list_maker**:  *(state -> action list)  -> state -> unit -> action option*
**of_array_maker**: *(state -> action array) -> state -> unit -> action option*
If you have a function f that returns a list (or array) of actions, `of_list_maker f` (or `of_array_maker f`) will return a thunk generator.

- **of_parser_maker**: *(state -> int -> action option) -> state -> unit -> action option*
The user-supplied function takes a state and returns a function that can be passed to `Stream.from` to create an action stream.

- **of_stepper_and_value_maker**:
&nbsp;&nbsp;&nbsp;&nbsp;*(state -> ('c -> (action \* 'c) option) * 'c) -> state -> unit -> action option*
The user-supplied function returns a function and an initial value that can be passed to `Seq.unfold` to create an action sequence.

- **roll your own**
You can think of a thunk generator as a generator of parsers that don't take integers, or a generator of stepping functions that don't need explicit initial values.

**SMA_star**
- **Make**: *Typeof_Problem -> Typeof_Search*
`Make (My_problem)` creates a module that contains the search function, using the DEPQ module included in the package.

- **search**: *queue_size: int -> ?max_depth: int -> Prob.state -> (Prob.action * Prob.state) list option*
`My_search.search ~queue_size:n s0` creates a double-ended priority queue and uses the SMA\* algorithm to find a goal state. The return value is an option containing a list of pairs (actions and states) that describe a path leading from the initial state to the goal state.


- **Make_with_queue**: *Typeof_Queue -> Typeof_Problem -> Typeof_Search*
`Make_with_queue (My_queue) (My_problem)` accepts a user-supplied queue module in addition to the problem. The queue is expected to inform nodes whenever their locations change and allow repositioning of any parent node whose f-cost increases (this happens when the known minimum f-cost of its successors increases). 

