(*
 * miscellaneous utility functions
 * copyright (c) 2021 Daniel S. Bensen
 *)

open Printf

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
  let n = ref (n0-1)
  and spares = ref [] in
  let reclaim m = spares := m :: !spares
  and issue () = match !spares with
    | [] -> n := !n + 1; !n
    | m::ms -> spares := ms; m
  in issue,reclaim

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

let intlog2 n =
  if n < 1 then invalid_arg (sprintf "intlog2: arg = %d (must be positive)" n)
  else
  let rec aux acc n =
    if n = 1 then acc
    else aux (acc+1) (n/2)
  in aux 0 n

module A = struct

  include Array

  let swap_cells b n1 n2 =
    let tmp = b.(n1) in
    b.(n1) <- b.(n2);
    b.(n2) <- tmp

end


module L = struct

  include List

  let extract_if test xs =
    let rec aux acc = function
      | x::xs -> if test x
                 then Some (x, rev_append acc xs)
                 else del (x::acc) xs
      | [] -> None
      in aux [] xs

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

module Ascii_art = struct

  let pad_string width str =
    let strwidth = String.length str in
    assert (width >= strwidth);
    let padding = width - strwidth in
    let pleft = padding/2 in
    let pright = padding - pleft in
    String.make pleft ' ' ^ str ^ String.make pright ' '

  let print_string_line strs = L.iter print_string strs; print_newline()

  let print_picture_row strss = L.iter print_string_line (L.transpose strss)

  let print_row width to_strings xs =
    let pad_row strss = L.map (L.map (pad_string width)) strss
    in print_picture_row (pad_row (L.map to_strings xs))

  let print_rows ~item_width ~items_per_row to_strings xs = 
    if items_per_row < 1 then invalid_arg
      (sprintf "print_rows: objects per row = %d (must be positive)" items_per_row)
    else let rec aux xs =
      let row,rest = L.snip items_per_row xs in
      if row <> [] then begin
        print_row item_width to_strings row;
        if rest <> [] then (print_newline(); aux rest)
      end
    in aux xs

end

