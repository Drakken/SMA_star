

To create a node's children (successors):

If the children will be stored in an array, define a function that
creates the array of child nodes and pass that function to Children.of_array:

let make_child_array p = ..... (* create and return an array containing p's child nodes *)

~init_children:(Children.of_array make_child_array)
  
If the children will be stored in a list, define a function that
creates the list of child nodes and pass that function to Children.of_list:

let make_child_list p = ..... (* create and return a list containing p's child nodes *)

~init_children:(Children.of_list make_child_list)


If the children will be generated from a stream, you'll need to define a function that
creates the stream of child nodes and returns the stream's parsing function.
The parsing function must take one integer argument and return an option, i.e.
parse n returns Some x, where x is the nth element of the stream, and None when the stream is empty.
n will be incremented each time parse is called, starting from zero.
Then pass the function you just defined to Children.of_stream:

let make_child_stream p = ..... (* create a stream of p's child nodes and return the stream's parsing function *)

~init_children:(Children.of_stream make_child_stream)


If the children will be generated from a sequence, you'll need to define a function that
creates the sequence of child nodes and and returns a tuple containing the stream's step function
and initial value.
The step function must take the initial value (s) of the sequence as its argument and return an option
containing the next element in the sequence (x) and the initial value (s') for the next call to the step function:
step s returns Some (x,s') until the end of the sequence, then returns None.

Then pass the function you just defined to Children.of_sequence:

let make_child_sequence p = .....
  (* create a sequence of p's child nodes and return the sequence's step function and initial value *)

~init_children:(Children.of_sequence make_child_sequence)




