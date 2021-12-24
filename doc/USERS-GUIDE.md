<br>

## SMA_star user's guide

Follow these steps to do an SMA* search with SMA_star:
1. Create a module of type *Typeof_Problem* (see below) that describes the problem you want to solve:<br>
`module My_problem = struct ... (* put the rules of the problem here *) ... end`
2. Create a search module by passing your problem module to `Make`:<br>
`module My_search = SMA_star.Make (My_problem)`
3. Start the search by calling `My_search.search`.
4. If your problem has a solution, the search will return an option that contains a list describing the path of actions and their resulting states that leads from the initial state you provided to a goal state.

The following elements are defined in SMA_star:

 ***Typeof_Problem***
<br>
the type of user-supplied modules that define search applications and can be passed to `SMA_star.Make`. A module of this type must completely describe the space of states  to be searched.

- ***state***<br>
the type of states that represent the results of full or partial solutions to the problem (including the initial state)

- ***action***<br>
the type of information required to specify an action that leads from a given state to one of its successors

- **trivial_action**: *action*<br>
a simple, arbitrary value of type *action*. It nominally leads to the starting state, but it's never used, so you can give it any value you want. Its only purpose is to make the search code use slightly less memory.

- **next_state**: *state -> action -> state*<br>
`next_state s a` returns the successor state that results from taking the action `a` with state `s` as the starting point.

- **make_action_generator**: *state -> action Generator.t*<br>
a function that generates a thunk to produce actions from a state. See Generator below. You'll need to provide SMA_star with a way to produce all of the actions that are allowed from a given state.

- **is_goal**: *state -> bool*<br>
`is_goal s` determines whether s is an acceptable goal state.

- **delta_g_cost_of_action**: *state -> action -> int*<br>
`delta_g_cost_of_action s a` returns the incremental cost (i.e. the increase in the g-cost) incurred by applying action `a` to state `s`.

- **h_cost_to_goal**: *state -> int*<br>
a permissible heuristic function for the problem. `h_cost_to_goal s` estimates the minimum possible cost to reach a goal state from state `s`.

**Generator**<br>
a module for creating thunk generators. For each state encountered in a search, SMA_star requires a thunk that returns an allowable action from that state each time it's called. The function `My_problem.make_action_generator` takes the state and returns the thunk, and the functions in this module convert the supplied generator (or related function) into a thunk generator. 

These functions are actually polymorphic, but, for the sake of concreteness, the type variables in the descriptions below have been replaced with `state` and `action` as defined in the user module. For the real signatures, replace `state` with `'a` and `action` with `'b`.

- ***action t*** = *unit -> action option*<br>
the type of action generators, i.e. thunks that return actions. An action generator returns `Some a` as long as there's at least one action (`a`) that hasn't been generated yet, and then returns `None`.

- **of_list_maker**:&nbsp; *(state -> action list)  -> state -> unit -> action option*<br>
**of_array_maker**: *(state -> action array) -> state -> unit -> action option*<br>
If you have a function `f` that returns a list (or array) of actions, `of_list_maker f` (or `of_array_maker f`) will return a thunk generator.

- **of_parser_maker**: *(state -> int -> action option) -> state -> unit -> action option*<br>
The user-supplied function takes a state and returns a function that can be passed to `Stream.from` to create an action stream.

- **of_stepper_and_value_maker**:<br>
&nbsp;&nbsp;&nbsp;&nbsp;*(state -> ('c -> (action \* 'c) option) * 'c) -> state -> unit -> action option*<br>
The user-supplied function returns a function and an initial value that can be passed to `Seq.unfold` to create an action sequence.

- **roll your own**<br>
You can think of a thunk generator as a generator of parsers that don't take integers, or a generator of stepping functions that don't need explicit initial values.

**Make**: *Typeof_Problem -> Typeof_Search*<br>
`Make (My_problem)` creates a module that contains the search function, using the DEPQ module included with SMA_star.

- **search**:<br>
&nbsp;&nbsp;&nbsp;&nbsp; *queue_size: int -> ?max_depth: int -> My_problem.state -> <br>
&nbsp;&nbsp;&nbsp;&nbsp; (My_problem.action * My_problem.state) list option*<br>
`My_search.search ~queue_size:n s0` creates a double-ended priority queue that can hold up to `n` nodes, and uses the SMA\* algorithm to find a path leading from the initial state `s0` to a goal state. The return value is an option containing a list of pairs (actions and states) that describe the path that was found.

**Make_with_queue**: *Typeof_Queue -> Typeof_Problem -> Typeof_Search*<br>
`Make_with_queue (My_queue) (My_problem)` accepts a user-supplied queue module in addition to the problem. The queue is expected to inform nodes whenever their locations change and allow repositioning of any parent node whose f-cost has increased. (This happens when the known minimum f-cost of its successors increases.)

**Utils.Ascii_art**
<br>
utility functions for displaying the solution path on the command line
- **print_row**: *int -> ('a -> string list) -> 'a list -> unit*<br>
**print_rows**: *item_width: int -> items_per_row: int -> ('a -> string list) -> 'a list -> unit*<br>
print one or more rows of ascii pictures. You can use these functions to display the solution path on the command line. An *('a -> string list)* argument passed to `print_row` or `print_rows` should return an ascii picture of its argument, represented as a list of character strings, one string per line. 

<br>
copyright (c) 2021 Daniel S. Bensen
<br><br>

