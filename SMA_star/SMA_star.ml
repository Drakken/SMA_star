(*
 * SMA* search algorithm
 * from S. Russell & P. Norvig, Artificial Intelligence: A Modern Approach
 * copyright (c) 2021 Daniel S. Bensen
 *)


open Printf
open Utils

let new_id = make_counter 0


module Generator = struct

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

end


module type Typeof_Problem = sig

  type state
  type action

  val trivial_action: action

  val next_state: state -> action -> state

  val make_action_generator: state -> action Generator.t

  val is_goal: state -> bool

  val dg_cost_of_action: state -> action -> int
  val h_cost_to_goal: state -> int

  val string_of_action: action -> string
  val string_of_state:  state  -> string

end



module type Typeof_Queue = sig

  module Make (E: Element.T): sig

    type element = E.t
    type t

    val make: int -> element -> t

    val size: t -> int

    val insert: t -> element -> unit

    val otop:    t -> element option
    val opop:    t -> element option
    val odrop:   t -> element option
    val obottom: t -> element option

    val is_full: t -> bool

    val update: t -> int -> unit

    val element_of_loc: t -> int -> element

  end

end


module type Typeof_Make =
  functor (Prob: Typeof_Problem) ->
  functor (Queue: Typeof_Queue) ->
    sig
      val search: queue_size: int -> ?max_depth: int -> Prob.state -> Prob.action list option
    end


module Make (Prob: Typeof_Problem)
            (Queue: Typeof_Queue)
    = struct

  module Node = struct

    type stub = { id: int; action: Prob.action; cost: int }

    type t =
    { id: int
     ;mutable loc: int
     ;parent: t
     ;depth: int
     ;action: Prob.action
     ;state: Prob.state
     ;gcost: int
     ;mutable fcost: int
     ;mutable next_action_opt: Prob.action Generator.t option
     ;mutable child_nodes: t list
     ;mutable child_stubs: stub list
    }
(*
    let print_node_stats fname n =
      printf "%s: id=%d. pid=%d. child nodes = " fname n.id n.parent.id;
      L.iter (fun n -> print_int n.id) n.parent.child_nodes;
      print_newline()
*)

    let id n = n.id

    let getloc n   = n.loc
    let setloc n i = n.loc <- i

    let stub_of_node n = { id = n.id; action = n.action; cost = n.fcost }

    let make_root_node state =
      let rec root =
      { id = new_id()
       ;loc = 0
       ;parent = root
       ;depth = 0
       ;action = Prob.trivial_action
       ;state
       ;next_action_opt = Some (Prob.make_action_generator state)
       ;gcost = 0
       ;fcost = Prob.h_cost_to_goal state
       ;child_nodes = []
       ;child_stubs = []
      }
    in root
 
    let do_parent n f =
      let p = n.parent in
      if p != n then f p

    let make_child_node id parent action state fcost gcost = 
    { id
     ;loc = 0
     ;parent
     ;depth = parent.depth + 1
     ;action
     ;state
     ;gcost
     ;fcost
     ;child_nodes = []
     ;child_stubs = []
     ;next_action_opt = Some (Prob.make_action_generator state)
    }

    let next_state p a = Prob.next_state p.state a

    let gcost p a = p.gcost + Prob.dg_cost_of_action p.state a

    let node_of_stub p {id;action;cost} =
      let state = next_state p action in
      make_child_node id p action state cost (gcost p action)

    let state_of_action p a max_depth = 
      let s = next_state p a in
      if Prob.is_goal s || p.depth < max_depth - 1
      then Some s
      else None

    let node_of_state p a s = 
      let gcost = gcost p a in
      let fcost = max p.fcost (gcost + Prob.h_cost_to_goal s)
      in make_child_node (new_id()) p a s fcost gcost

    let[@inline] no_fulls n = n.child_nodes = []
    let[@inline] no_stubs n = n.child_stubs = []
    let[@inline] no_nexts n = n.next_action_opt = None

    let[@inline] has_fulls_only  n = no_stubs n && no_nexts n
    let[@inline] has_no_children n = no_stubs n && no_nexts n && no_fulls n

    let insert_by_cost xs x =
      let rec ins rev_ys = function
        | y::ys as xs -> if x.cost <= y.cost
                         then L.rev_append rev_ys (x::xs)
                         else ins (y :: rev_ys) ys
        | [] -> L.rev_append rev_ys [x]
        in
        ins [] xs

    let min_node_cost ns = L.min_of (fun n -> n.fcost) ns 
     
    let min_child_node_cost n = min_node_cost n.child_nodes     

    let min_child_cost n =
      match n.child_nodes,
            n.child_stubs with
      | [],s::_ -> s.cost
      | ns, [] -> min_node_cost ns
      | ns,s::_ -> min s.cost (min_node_cost ns)

    let[@inline] delete_child p c =
    (*  try *)
        p.child_nodes <- L.remove_object c p.child_nodes
   (*   with _ ->
        printf  "child id = %d\n" c.id;
        printf "parent id = %d\n" p.id;
        print_string "children ids =";
        L.iter (fun c -> printf " %d" c.id) p.child_nodes;
        print_newline()
   *)


    let[@inline]   push_child p c = p.child_nodes <- c :: p.child_nodes
    let[@inline] insert_stub  p s = p.child_stubs <- insert_by_cost p.child_stubs s

    let[@inline] delete_child_node c = delete_child c.parent c
    let[@inline]    add_child_node c =   push_child c.parent c
    let[@inline]    add_child_stub c = insert_stub  c.parent (stub_of_node c)

    let beats a b = a.fcost < b.fcost || (a.fcost = b.fcost && a.depth > b.depth)

  end


  (************************************* SEARCH *************************************)

  module Q = Queue.Make (Node)

  open Node

  let otop q = Q.otop q (* try Some (Q.top q) with _ -> None *)

  let pop q = from_some (Q.opop q)

 (* let iggypop q = ignore (pop q) *)

  let[@inline] not_full q = not (Q.is_full q)

  let bottom q =
    match Q.obottom q with
    | Some x -> x
    | None -> invalid_arg "bottom: queue is empty"

  let rec
    stubify_node q i n = print_int i; do_parent n (stubify_old_child q n)
  and
    stubify_old_child q c p = 
      let p_had_fulls_only = has_fulls_only p in          (* not in the queue *)
      if p_had_fulls_only then assert (p.loc = 0);
      delete_child_node c;
      add_child_stub c;
      if p_had_fulls_only then assert (try_to_insert_old q p)
  and
    drop_and_try_old_again q n =
      Q.odrop q >>=! stubify_node q 1;
      try_to_insert_old q n
  and
    try_to_insert_old q n =
      if not_full q then (Q.insert q n; true)
      else if beats n (bottom q) then drop_and_try_old_again q n
      else (stubify_node q 2 n; false)

  let rec try_to_insert_new q n =
      if not_full q then (Q.insert q n; true)
      else beats n (bottom q)
           && match Q.odrop q with
              | None -> failwith "couldn't drop q"
              | Some b -> printf "\nttin: q size = %d, bottom id = %d ... sn ..." (Q.size q) b.id;
                          stubify_node q 3 b;
                          printf " q size = %d, bottom id = %d\n" (Q.size q) (from_some (Q.obottom q)).id;
                          try_to_insert_new q n

  let do_next_stub p q =
    assert (not (no_fulls p));
    match p.child_stubs with
    | [] -> assert (pop q == p);             (* immediately after actions run out *)
    | x::xs ->
        fail_if (min_child_node_cost p <= p.fcost) "the cheapest child should be the top";
        let n = node_of_stub p x in
        if xs = [] then assert (pop q == p);   
        fail_if (not (try_to_insert_new q n)) "stub cost should equal parent cost";
        p.child_stubs <- xs;
        add_child_node n


  let rec update_fcost q n =
    let cmin = min_child_cost n in
    if cmin > n.fcost
    then begin
      n.fcost <- cmin;
      if n.loc <> 0 then Q.update q n.loc;
      do_parent n (update_fcost q)
    end

  let rec delete_child n =
    assert (n.loc = 0); 
    delete_child_node n;
    do_parent n (fun p -> if has_no_children p then delete_child p)


  let do_next_action next_action n q max_depth =
    let do_next () =
      match next_action () with
      | Some a -> begin
          match state_of_action n a max_depth
          with
          | Some s -> let c = node_of_state n a s in 
                      if try_to_insert_new q c
                      then add_child_node c
                      else add_child_stub c  (* do_next ()) *)
          | None -> () (* do_next ()  *)                  (* no child = infinite cost *)
        end
      | None ->                             (* all children have been generated *)
          n.next_action_opt <- None;
          if no_stubs n then assert (pop q == n);
          if no_stubs n
          && no_fulls n then delete_child n
          else update_fcost q n
    in do_next ()


  let do_next_child p q max_depth =
    match p.next_action_opt with
    | Some f -> print_char 'a'; do_next_action f p q max_depth
    | None   -> print_char 's'; do_next_stub     p q

  let action_path n =
    let rec do_node acc n =
      let p = n.parent in
      if p == n then acc
      else do_node (n.action :: acc) p
    in do_node [] n

  let print_node n =
    printf "\nprint_node: id=%d. pid=%d. depth=%d. gcost=%d. fcost=%d. %d fulls. %d stubs. Next action? %c.\n"
          n.id  n.parent.id  n.depth   n.gcost   n.fcost
          (L.length n.child_nodes)
          (L.length n.child_stubs) (if n.next_action_opt = None then 'n' else 'y');
    printf "action: %s\n" (Prob.string_of_action n.action);
    printf  "state: %s\n" (Prob.string_of_state  n.state)

  let search ~queue_size ?max_depth state =
    let root = make_root_node state in
    let q = Q.make queue_size root in
    let max_depth =
      match max_depth with
      | None -> queue_size - 1
      | Some d ->
          if d < queue_size then d
          else invalid_arg "max depth exceeds queue size"
    in
    let rec loop i n = print_char '.';
(*
      if true then printf " id%d " n.id
      else (print_node n; ignore (read_line()))
;
*)
      assert (not (has_no_children n));
      if Prob.is_goal n.state then Some (action_path n)
      else (do_next_child n q max_depth;
            otop q >>= loop ((i+1) mod 20))
    in
    Q.insert q root;
    print_node root;
    loop 0 root

end
