(*
 * miscellaneous utility functions
 * copyright (c) 2021 Daniel S. Bensen
 *)


exception Not_found_in of string

let not_found_in str = raise (Not_found_in str)

let fail_if x str = if    x    then failwith str
let verify  x str = if (not x) then failwith str

let try_or_die f x str = try f x with _ -> failwith str

let (>>= ) xo f = match xo with Some x -> f x | None -> None
let (>>=!) xo f = match xo with Some x -> f x | None -> ()

let from_some = function
  | None -> invalid_arg "None in from_some"
  | Some x -> x

let make_counter n0 =
  let n = ref (n0-1) in
  fun () -> n := !n + 1; !n

let fold_times f x0 n =
  let rec fold x i = if i=0 then x else fold (f x) (i-1)
  in fold x0 n

let is_even n = ( n  = 2*(n/2))
let is_odd  n = (n-1 = 2*(n/2))

let rec demand_int msg =
  print_string msg;
  match read_int_opt () with
  | None -> print_endline "That's not an integer."
            ;demand_int msg
  | Some n -> n

let rec ( ** ) x n =
  if n < 0 then invalid_arg "negative exponent"
  else if n = 0 then 1
  else if n = 1 then x
  else if is_even n then (x*x)**(n/2)
  else               x * (x*x)**(n/2)


module A = struct

  include Array

  let swap_cells b n1 n2 =
    let tmp = b.(n1) in
    b.(n1) <- b.(n2);
    b.(n2) <- tmp

end


module L = struct

  include List

  let remove_if f xs =
    let rec del rev_xs = function
      | x::xs -> if f x
                 then rev_append rev_xs xs
                 else del (x :: rev_xs) xs
      | [] -> not_found_in "remove_if"
      in
      del [] xs

  let remove_object x xs = remove_if ((==) x) xs

  let min_of f = function
  | x::xs -> fold_left (fun a y -> min a (f y)) (f x) xs
  | [] -> invalid_arg "min_of: empty list"

  let sum = function
    | x::xs -> fold_left (+) x xs
    | [] -> invalid_arg "sum: empty list"

  let combine f xs = 
    match xs with
    | [] ->  invalid_arg "can't combine empty list"
    | x::xs -> fold_left f x xs  

  let interleave f z = combine (fun acc x -> f (f acc z) x)

  let rec transpose xss =
    if      for_all ((=)  []) xss then []
    else if for_all ((<>) []) xss then map hd xss :: transpose (map tl xss)
    else invalid_arg "transpose: lists have different lengths"

  let snip n xs = 
    let rec xfer n xs1 = function
      | [] -> (rev xs1, [])
      | x::xs2 -> if n = 0 then (rev xs1, x::xs2)
                  else xfer (n-1) (x::xs1) xs2
    in xfer n [] xs

end

let print_strings strs = L.iter print_string strs; print_newline()

let print_lines to_strings xs =
  L.(iter print_strings (xs |> map to_strings |> transpose))
  
