(*
 * sliding puzzle (e.g. 15 puzzle, etc.)
 * test case for SMA_star
 * copyright (c) 2021 Daniel S. Bensen
 *)
(*
let debugging = true

type ('a,'b) debug_params = DP of ('a -> 'b) * 'a

let make_debug debugging =
  if debugging
  then fun (DP (f,x)) -> ignore (f x)
  else fun _ -> ()

let debug = make_debug debugging
*)

open Printf

let third (_,_,x) = x


module A = struct

  include Array

  let swap_cells b n1 n2 =
    let tmp = b.(n1) in
    b.(n1) <- b.(n2);
    b.(n2) <- tmp

end


module L = struct

  include List

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


module type Typeof_Params = sig
  val size: int
end

module Puzzle (Params: Typeof_Params) = struct

  open Params

  let length = size*size

  type state = { n_empty: int; board: int array }

  type action = Above | Below | Right | Left

  let trivial_action = Above 

  let char_of_action = function
    | Above -> 'A'
    | Below -> 'B'
    | Right -> 'R'
    | Left  -> 'L'

  let[@inline] rowcol n = n / size, n mod size

  let[@inline] index (row,col) = col + row*size

  let next_state { n_empty; board } side
    =
    let make_board r c
      =
      let nnew = index (r,c) in
      let new_board = A.copy board in
      A.swap_cells new_board n_empty nnew;
      { n_empty = nnew;
        board = new_board }
    in
    let vertical dr 
      =
      let r0,c0 = rowcol n_empty in
      let rnew = r0 + dr in
      assert (rnew >= 0 || rnew < size);
      make_board rnew c0
    and
        horizontal dc
      =
      let r0,c0 = rowcol n_empty in
      let cnew = c0 + dc in
      assert (cnew >= 0 || cnew < size);
      make_board r0 cnew
    in
    match side with
    | Above ->  vertical  (-1)
    | Below ->  vertical    1
    | Left  -> horizontal (-1)
    | Right -> horizontal   1

  let is_goal {n_empty;board} =
    let num_vals = length - 1 in
    n_empty = num_vals &&
    let is_okay n = board.(n) = n+1
    in let ns = L.init num_vals Fun.id
    in L.for_all is_okay ns

  let dg_cost_of_action _ _ = 1

  let h_cost_to_goal {n_empty;board} = 
    let distance ncell =
      if ncell = n_empty then 0
      else
      let ntile = board.(ncell) in
      let rcell,ccell = rowcol ncell in
      let rtarget,ctarget = rowcol (ntile - 1) in
      abs (rcell - rtarget) +
      abs (ccell - ctarget)
    in let cellnums = L.init (A.length board) Fun.id
    in L.(sum (map distance cellnums))

  let actions {n_empty;_} =
    let on_board (row,col,_) =
      row >=0 && row < size && 
      col >=0 && col < size
    in
    (* print_int n_empty; flush stdout; *)
    let row,col = rowcol n_empty
    in (* print_rowcol row col
    ;  *) let rcas = [
      (row-1,col,Above);
      (row+1,col,Below);
      (row,col-1,Left);
      (row,col+1,Right)] in
    let actns = L.(map third (filter on_board rcas))
    in (* print_newline(); print_actions actns;*) actns

  let make_action_generator = SMA_star.Generator.of_list actions

  let string_of_action = function
    | Above -> "Above"
    | Below -> "Below"
    | Right -> "Right"
    | Left  -> "Left"

  let string_of_row board r =
    let n0 = index (r,0) in
    A.(sub board n0 size |> map (sprintf "%3d")
                         |> fold_left (^) "")

  let string_of_state {n_empty;board} =
    let r,c = rowcol n_empty in
    let n_emptystr = sprintf "n_empty = %d: (%d,%d)\n" n_empty (r+1) (c+1) in
    let boardstr =
      L.(init size Fun.id |> map (string_of_row board)
                          |> interleave (^) "\n")
    in n_emptystr ^ boardstr

  let make_solution ()
    = printf "array length = %d\n" length
    ;  let board = A.init length ((+) 1)
    in let nlast = length-1 in
    board.(nlast) <- 0;
    { n_empty = nlast; board }

  let make_random_move s
    =  let actions = actions s
    in let num_actions = L.length actions
    in (*print_int num_actions
    ;  *)let a = L.nth actions (Random.int num_actions)
    in next_state s a

  let find_blank b =
    let rec find n =
      if n = length then raise Not_found
      else if b.(n) = length then n
      else find (n+1)
    in find 0

  let make_random_board () =
    let board = A.init length ((+) 1)
    in let swap ns =
      let len = L.length ns in
      let n1 = L.nth ns (Random.int len) in
      let ns = L.filter ((<>) n1) ns in
      let n2 = L.nth ns (Random.int (len-1)) in
      board.(n1) <- n2+1;
      board.(n2) <- n1+1; L.filter ((<>) n2) ns
    in let rec swap2 ns = if L.length ns >= 4 then swap2 (swap (swap ns))
    in swap2 (L.init length Fun.id);
    let n_empty = find_blank board
    in {n_empty;board}

  let string_of_rowcol r c = sprintf "(%d,%d)" (r+1) (c+1)

  let print_actions actns = L.(iter print_char (map char_of_action actns))

  let tilestr n = if n = length then " " else string_of_int n

end


let rec demand_int msg =
  print_string msg;
  match read_int_opt () with
  | None -> print_endline "That's not an integer."
            ;demand_int msg
  | Some n -> n

(* 
let print_stuff size
   =  printf "Board size = %d\n" n
  ;  let nrand = demand_int "Number of moves to randomize starting state? "
  in printf "nrand = %n\n" nrand
  ;  let solution = make_solution ()
  in let root = L.fold_times make_random_move solution nrand
*)

let test ~queue_size =
  let n = 3 (*demand_int "Board size? " *)
  in
  let module Params = struct let size = n end in
  let module Puzl = Puzzle (Params) in
  let module Search = SMA_star.Make (Puzl) (TestQ)
  in
  (* print_stuff size; *)
  let root = Puzl.make_random_board ()
  in print_endline "\nStarting state:"

  ;  match Search.search ~queue_size root with
     | None -> print_endline "No solution."
     | Some path 
  -> let pathchars = L.map Puzl.char_of_action path
  in L.iter print_char pathchars

  ;  print_newline ()


;;test ~queue_size:100



(*

let print_rows r boards =
  let space_width = 3
  in let num_boards = L.length boards
  in let num_chars = num_boards * !size + (num_boards - 1) * space_width
  in let board_str = String.make num_chars ' '
  in let print_row nb nr =
       let n_empty = index (r,0)
       in for i = 0 to (!size - 1) do print_tile (board.(n_empty+i)) done
  ; print_newline()

let print_boards boards =
  for i = 0 to (!size - 1) do print_rows i boards done

;; print_board (make_random_board 3)
;; print_board (make_random_board 3)
;; print_board (make_random_board 3)
;; print_board (make_random_board 3)
*)



