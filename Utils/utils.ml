(*
 * miscellaneous utility functions
 * copyright (c) 2021 Daniel S. Bensen
 *)


exception Not_found_in of string

let not_found_in str = raise (Not_found_in str)

let (>>= ) xo f = match xo with Some x -> f x | None -> None
let (>>=!) xo f = match xo with Some x -> f x | None -> ()

let from_some = function
  | None -> invalid_arg "None"
  | Some x -> x

let make_counter n0 =
  let n = ref (n0-1) in
  fun () -> n := !n + 1; !n

let new_id = make_counter 0


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
  | [] -> invalid_arg "empty list"

  let sum = function
    | x::xs -> fold_left (+) x xs
    | [] -> invalid_arg "sum: empty list"

  let combine f xs = 
    match xs with
    | [] ->  invalid_arg "can't combine empty list"
    | x::xs -> fold_left f x xs  

  let interleave f z = combine (fun acc x -> f (f acc z) x)

  let fold_times f x0 n =
    let rec fold x i = if i=0 then x else fold (f x) (i-1)
    in fold x0 n

end
