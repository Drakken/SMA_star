
## SMA_star ##

This is the README file for SMA_star, an OCaml implementation of the SMA* search algorithm developed by Stuart Russell.

copyright (c) 2021 Daniel S. Bensen

**UPDATE**: The search function seems to be working better now. It can solve a fully randomized 8 puzzle (3x3 sliding-tile puzzle). 

The code mostly follows the design described in *Artificial Intelligence: A Modern Approach*, by Stuart Russell and Peter Norvig. High-cost nodes that don't fit in the queue are retained as stubs that contain the node's action and f-cost. This enables the algorithm to regenerate the most promising nodes first. Ineligible nodes, i.e. those with "infinite" cost, are deleted completely; they can't be regenerated unless the parent node is deleted and regenerated.

SMA_star uses utility functions in the package SMA_star_utils, and is packaged together with SMA_star_puzzle, a command-line program that solves sliding puzzles.

See doc/USERS-GUIDE.md for details, and puzzle/puzzle.ml for an example.


## Synopsis ##

	module My_problem =
    	struct
			.
			.  (* define the states, actions, and required values here *)
			.
		end

	module My_search = SMA_star.Make (My_problem)
    
    let path_opt = My_search.search ~queue_size:my_queue_size my_initial_state

## History ##

### 2021 ###

**Dec 27** Search solved an 8-puzzle starting from a random board.

**Dec 24** Added state database to avoid duplicating states. 
