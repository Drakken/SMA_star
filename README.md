
This is the README file for SMA_star, an OCaml implementation of the SMA* search algorithm.

**WARNING**: The code is still very buggy. So far, the sliding puzzle only works on states that
are within a few moves of the solution.

The code mostly follows the design described in *Artificial Intelligence: A Modern Approach*, 
by Stuart Russell and Peter Norvig. High-cost nodes that don't fit in the queue are retained as 
stubs that contain the node's action and f-cost. This enables the algorithm to regenerate the most 
promising nodes first. Ineligible nodes, i.e. those with "infinite" cost, are deleted completely; 
they can't be regenerated unless the parent node is deleted and regenerated.

SMA_star uses utility functions in the package SMA_star_utils,
and is used by SMA_star_puzzle, a command-line program that solves sliding puzzles.

See doc/user-guide.md for details, and puzzle/puzzle.ml for an example.

