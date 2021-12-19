(*
 * thunk generators for SMA_star
 * copyright (c) 2021 Daniel S. Bensen
 *)

type 'a t = unit -> 'a option

let of_stream_function f =
  let open Stream in
  let s = from f in 
  fun () -> try Some (next s) with Failure -> None

let of_parser_maker make_f p = of_stream_function (make_f p)

let of_array_maker make_array p =
  let a = make_array p in
  let l = Array.length a in
  of_stream_function (fun n -> if n<l then Some a.(n) else None)

let of_sequence_function (f,x0) =
  let open Seq in
  let sref = ref (unfold f x0) in
  fun () ->
    match !sref () with
    | Nil -> None
    | Cons (x,s) -> sref := s; Some x

let of_stepper_and_value_maker make_fx p = of_sequence_function (make_fx p)

let of_list_maker make_list p =
  let l = make_list p in
  let f = function
    | x::xs -> Some (x,xs)
    | [] -> None
  in
  of_sequence_function (f,l)
