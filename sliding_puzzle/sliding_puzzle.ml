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

open Utils

let new_id = make_counter 0

module type Typeof_Params = sig
  val size: int
  val solution: int array
end

module Puzzle8_params = struct
  let size = 3
  let solution =
    [|1;2;3;
      8;0;4;
      7;6;5|]
end

module Puzzle (Params: Typeof_Params) = struct

  open Params

  type state = { n0: int; board: int array }

  let length = size*size

  ;; assert (A.length solution = length)

  let target_indexes = A.make length (-1)
  ;;
  A.iteri (fun i x -> target_indexes.(x) <- i) solution

  ;;assert (L.for_all (fun n -> solution.(target_indexes.(n)) = n)
                       (L.init length Fun.id))

  let solution = 
   { n0 = target_indexes.(0);
     board = solution }
    
  let[@inline] rowcol n = 
    let row = n / size
    and col = n mod size
    in (* printf " n%d,r%d,c%d " n row col; *)
    row,col

  let target_rowcols = A.map rowcol target_indexes

  let[@inline] index (row,col) = col + row*size

  type action = Above | Below | Right | Left

  let trivial_action = Above 

  let char_of_action = function
    | Above -> 'A'
    | Below -> 'B'
    | Right -> 'R'
    | Left  -> 'L'

  let string_of_action = function
    | Above -> "Above"
    | Below -> "Below"
    | Right -> "Right"
    | Left  -> "Left"

  let next_state { n0; board } side
    =
    let make_board r c
      =
      let nnew = index (r,c) in
      let new_board = A.copy board in
      assert (new_board.(n0) = 0);
      A.swap_cells new_board n0 nnew;
      { n0 = nnew;
        board = new_board }
    in
    let vertical dr 
      =
      let r0,c0 = rowcol n0 in
      let rnew = r0 + dr in
      assert (rnew >= 0 || rnew < size);
      make_board rnew c0
    and
        horizontal dc
      =
      let r0,c0 = rowcol n0 in
      let cnew = c0 + dc in
      assert (cnew >= 0 || cnew < size);
      make_board r0 cnew
    in
    match side with
    | Above ->  vertical  (-1)
    | Below ->  vertical    1
    | Left  -> horizontal (-1)
    | Right -> horizontal   1

  let is_goal {board;_} =
    let is_okay n = board.(n) = solution.board.(n)
    in L.for_all is_okay (L.init length Fun.id)

  let delta_g_cost_of_action _ _ = 1

  let h_cost_to_goal {n0;board} = 
    let distance ncell =
      if ncell = n0 then (assert (board.(n0) = 0); 0)
      else
      let ntile = board.(ncell) in
      let rcell,ccell = rowcol ncell in
      let rtarget,ctarget = target_rowcols.(ntile) in
      abs (rcell - rtarget) +
      abs (ccell - ctarget)
    in let cellnums = L.init length Fun.id
    in L.(sum (map distance cellnums))

  let print_rowcol r c = printf "(%d,%d)" r c

  let actions {n0;_} =
    let on_board (row,col,_) =
      row >=0 && row < size && 
      col >=0 && col < size
    in
    (* print_int n0; *)
    let row,col = rowcol n0
    in  (* print_rowcol row col
    ;   *) let rcas = [
      (row-1,col,Above);
      (row+1,col,Below);
      (row,col-1,Left);
      (row,col+1,Right)] in
    let actns = L.(map third (filter on_board rcas))
    in (* print_newline(); print_actions actns; *)
    actns

  let make_action_generator = SMA_star.Generator.of_list_maker actions

  let tilestr  n = if n = 0 then  " "  else string_of_int n

  let tilestr3 n = if n = 0 then "   " else sprintf "%3d" n

  let string_of_row board r =
    let n0 = index (r,0) in
    A.(sub board n0 size |> map tilestr3
                         |> fold_left (^) "")

  let string_of_state {board;_} =
(*
    let r,c = rowcol n0 in
    let n0str = sprintf "r0=%d,c0=%d\n" (r+1) (c+1) in
*)
    let boardstr =
      L.(init size Fun.id |> map (string_of_row board)
                          |> interleave (^) "\n")
    in (* n0str ^ *) boardstr

  let make_random_move s
    =  let actions = actions s
    in let num_actions = L.length actions
    in (*print_int num_actions
    ;  *)let a = L.nth actions (Random.int num_actions)
    in next_state s a

  let find_blank b =
    let rec find n =
      if n = length then not_found_in "find_blank"
      else if b.(n) = length then n
      else find (n+1)
    in find 0

  let make_random_board () =
    let board = A.copy solution.board
    in let swap ns =
      let len = L.length ns in
      let n1 = L.nth ns (Random.int len) in
      let ns = L.filter ((<>) n1) ns in
      let n2 = L.nth ns (Random.int (len-1)) in
      let tmp = board.(n1) in
      board.(n1) <- board.(n2);
      board.(n2) <- tmp;
      L.filter ((<>) n2) ns
    (* in let rec swap2 ns =
         if L.length ns >= 4 then swap2 (swap (swap ns)) *)      (* even permutations only! *)
    in let swap2 ns = ignore (swap (swap ns))
    in swap2 (L.filter ((<>) solution.n0) (L.init length Fun.id))
    ; { n0 = solution.n0; board }

  let string_of_rowcol r c = sprintf "(%d,%d)" (r+1) (c+1)

  let print_actions actns = L.(iter print_char (map char_of_action actns))

  let strings_of_state {board;_} = L.map (string_of_row board) (L.init size Fun.id)

  let padded_string_of_action = function
             | Above -> "  above ->"
             | Below -> "  below ->"
             | Right -> "  right ->"
             | Left  -> "  left  ->"
  let action_padding =  "          "

  let strings_of_action a =
    let    n1 = (size-1)/2
    in let n2 = (size-1) - n1
    in let strs1 = L.init n1 (fun _ -> action_padding)
    in let strs2 = L.init n2 (fun _ -> action_padding)
    in strs1 @ (padded_string_of_action a :: strs2)

  let strings_of_pair (a,s) = L.map2 (^) (strings_of_action a) (strings_of_state s)

  let page_width = 100

  let boards_per_page = page_width / (10 + 3*size)

  let print_path_row pairs = print_lines strings_of_pair pairs

  let rec print_path pairs = 
    let row,rest = L.snip boards_per_page pairs in
    if row  <> [] then print_path_row row;
    if rest <> [] then (print_newline(); print_path rest)

end


(* 
let print_stuff size
   =  printf "Board size = %d\n" n
  ;  let nrand = demand_int "Number of moves to randomize starting state? "
  in printf "nrand = %n\n" nrand
  ;  let solution = make_solution ()
  in let root = L.fold_times make_random_move solution nrand
*)

let test ~queue_size =
 (* let n = demand_int "Board size? " 
  in *) let module Puzl = Puzzle (Puzzle8_params)
  in let module Search = SMA_star.Make (Puzl) (TestQ)
  in
  (* print_stuff size; *)
  (* let _ = Puzl.make_random_board () in *)
  print_endline "\nWe'll test the search code by solving an 8 puzzle with various parameters."
  ;
  let test_board root msg =
    printf "\nStarting from %s:\n\n" msg;
    match Search.search ~queue_size root with
     | None -> print_endline "No solution."
     | Some path -> print_newline (); Puzl.print_path path;
                    print_newline ()
  in 
(*
  test_board Puzl.solution "an already solved board";

  test_board Puzl.(make_random_move solution) "one move away";
*)
  test_board Puzl.(fold_times make_random_move solution   5) "ten random moves away";
  test_board Puzl.(fold_times make_random_move solution   8) "18 random moves away";
(*
  test_board Puzl.(make_random_board ()) "two random cell swaps"
*)
;;test ~queue_size:1000000



(*

let print_rows r boards =
  let space_width = 3
  in let num_boards = L.length boards
  in let num_chars = num_boards * !size + (num_boards - 1) * space_width
  in let board_str = String.make num_chars ' '
  in let print_row nb nr =
       let n0 = index (r,0)
       in for i = 0 to (!size - 1) do print_tile (board.(n_empty+i)) done
  ; print_newline()

let print_boards boards =
  for i = 0 to (!size - 1) do print_rows i boards done

;; print_board (make_random_board 3)
;; print_board (make_random_board 3)
;; print_board (make_random_board 3)
;; print_board (make_random_board 3)
*)



