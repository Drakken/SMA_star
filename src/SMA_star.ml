(*
 * SMA* search algorithm
 * from S. Russell & P. Norvig, Artificial Intelligence: A Modern Approach
 * copyright (c) 2021 Daniel S. Bensen
 *)

(*
let page_width = 100
*)

open Printf
open Utils

module HT = Hashtbl

module Element   = Element
module Generator = Generator
module DEPQ      = DEPQ


let new_id, recycle_id = make_counter 0


module type Typeof_Problem = sig

  type state
  type action

  val trivial_action: action

  val next_state: state -> action -> state

  val make_action_generator: state -> action Generator.t

  val is_goal: state -> bool

  val delta_g_cost_of_action: state -> action -> int
  val h_cost_to_goal: state -> int

  val string_of_action: action -> string
  val string_of_state:  state  -> string

end


module Node (Prob: Typeof_Problem) = struct

  type stub = { id: int; action: Prob.action; cost: int }

  type t = {
    id: int;
    mutable loc: int;
    parent: t;
    depth: int;
    action: Prob.action;
    state:  Prob.state;
    gcost: int;
    mutable fcost: int;
    mutable next_action_opt: Prob.action Generator.t option;
    mutable fulls: t list;
    mutable stubs: stub list;
    mutable  dups: stub list;
  }

  let getloc n   = n.loc
  let setloc n i = n.loc <- i

  let to_stub n = recycle_id n.id; { action = n.action; cost = n.fcost }

  let make_root state =
    let rec root = {
      id = new_id();
      loc = 0;
      parent = root;
      depth = 0;
      action = Prob.trivial_action;
      state;
      next_action_opt = Some (Prob.make_action_generator state);
      gcost = 0;
      fcost = Prob.h_cost_to_goal state;
      fulls = [];
      stubs = [];
      dups  = [];
    }
    in root
 
  let do_parent n f =
    let p = n.parent in
    if p != n then f p        (* don't do the root node *)

  let make_child parent action state fcost gcost = 
    {
      id = new_id();
      loc = 0;
      parent;
      depth = parent.depth + 1;
      action;
      state;
      gcost;
      fcost;
      fulls = [];
      stubs = [];
      dups  = [];
      next_action_opt = Some (Prob.make_action_generator state);
    }

  let next_state p a = Prob.next_state p.state a

  let gcost p a = p.gcost + Prob.delta_g_cost_of_action p.state a

  let of_stub p {action;cost} =
      let state = next_state p action in
      make_child p action state cost (gcost p action)

  let state_of_action p a max_depth = 
      let s = next_state p a in
      if Prob.is_goal s || p.depth < max_depth - 1
      then Some s
      else None

  let of_state p a s = 
      let gcost = gcost p a in
      let fcost = max p.fcost (gcost + Prob.h_cost_to_goal s)
      in make_child p a s fcost gcost

  let[@inline] no_fulls n = n.fulls = []
  let[@inline] no_stubs n = n.stubs = []
  let[@inline] no_dups  n = n.dups  = []
  let[@inline] no_nexts n = n.next_action_opt = None

  let[@inline] no_stubs_or_dups n = no_stubs n && no_dups n

  let[@inline] has_fulls n = n.fulls <> []
  let[@inline] has_stubs n = n.stubs <> []
  let[@inline] has_dups  n = n.dups  <> []
  let[@inline] has_nexts n = n.next_action_opt <> None

  let[@inline] has_fulls_only  n =  no_stubs n &&  no_nexts n &&  no_dups n
  let[@inline] has_no_children n =  no_stubs n &&  no_nexts n &&  no_dups n &&  no_fulls n
  let[@inline]    has_children n = has_stubs n || has_nexts n || has_dups n || has_fulls n

  let insert_by_cost xs x =
      let rec ins rev_ys = function
        | y::ys as xs -> if x.cost <= y.cost
                         then L.rev_append rev_ys (x::xs)
                         else ins (y :: rev_ys) ys
        | [] -> L.rev_append rev_ys [x]
        in
        ins [] xs

  let min_opt cos = 
    match L.concat_map Option.to_list cos with
    | [] -> None
    | cs -> Some (L.min cs)

  let min_stub_cost = function
    | [] -> None
    | x::xs -> Some x.cost

  let min_full_cost = function
    | [] -> None
    | ns -> Some (L.min_of (fun n -> n.fcost) ns)
     
  let min_full_child_cost n = min_cost n.fulls     

  let min_child_cost n =
      min_opt [ min_full_cost n.fulls;
                min_stub_cost n.stubs;  
                min_stub_cost n.dups ]

  let[@inline] delete_full p c =
    (*  try *)
        p.fulls <- L.remove_object c p.fulls
   (*   with _ ->
        printf  "child id = %d\n" c.id;
        printf "parent id = %d\n" p.id;
        print_string "children ids =";
        L.iter (fun c -> printf " %d" c.id) p.fulls;
        print_newline()
   *)

  let[@inline]  push_child c = let p = c.parent in p.fulls <- c :: p.fulls
  let[@inline] insert_stub c = let p = c.parent in p.stubs <- insert_by_cost p.stubs (to_stub c)

  let[@inline] insert_dup p action cost = p.dups <- insert_by_cost p.dups {action;cost}

  let beats a b = a.fcost < b.fcost || (a.fcost = b.fcost && a.depth > b.depth)

  let rec delete_child c =
    assert (c.loc = 0); 
    delete_full c.parent c;
    do_parent c (fun p -> if has_no_children p then delete_child p)

  let indb db n = HT.mem db x.state

  (* let add2db db n = HT.add db c.state c *)

  let rec delete db n =
    delete_child n;
    recycle_id n.id;
    assert (HT.mem db n.state);
    HT.remove      db n.state

  let to_strings n =
    let    line0 = sprintf "id%d p%d d%d" n.id n.parent.id n.depth
    in let line1 = sprintf  "a%s g%d f%d" (Prob.string_of_action n.action) n.gcost n.fcost
    in
    let line2 = sprintf "c%d s%d n%c"
          (L.length n.fulls)
          (L.length n.stubs)
          (if n.next_action_opt = None then 'n' else 'y')     
    in
    let state_lines = String.split_on_char '\n' (Prob.string_of_state  n.state)
    in
    line0::line1::line2::state_lines
(*
  let print n = L.iter print_endline (to_strings n)
*)
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

    val print: t -> unit

  end

end


module type Typeof_Make =
  functor (Prob: Typeof_Problem) ->
    sig
      val search:
        queue_size:int ->
        ?max_depth:int ->
        Prob.state ->
        (Prob.action * Prob.state) list option
    end


module Make_with_queue (Queue: Typeof_Queue)
                       (Prob:  Typeof_Problem)
    = struct

  module N = Node (Prob)
  module Q = Queue.Make (N)

  let otop q = Q.otop q (* try Some (Q.top q) with _ -> None *)

  let pop q = from_some (Q.opop q)

  let[@inline] not_full q = not (Q.is_full q)

  let bottom q =
    match Q.obottom q with
    | Some x -> x
    | None -> invalid_arg "bottom: queue is empty"

  let rec
    stubify_node q n = N.do_parent n (stubify_old_child q n)
  and
    stubify_old_child q c p = 
      let open N in
      let p_had_fulls_only = has_fulls_only p in          (* not in the queue *)
      if p_had_fulls_only then assert (p.loc = 0);
      delete_child_node c;
      add_stub c;
      if p_had_fulls_only then assert (try_to_insert_old q p)
  and
    drop_and_try_old_again q n =
      Q.odrop q >>=! stubify_node q;
      try_to_insert_old q n
  and
    try_to_insert_old q n =
      if not_full q then (Q.insert q n; true)
      else if N.beats n (bottom q) then drop_and_try_old_again q n
      else (stubify_node q n; false)

  let rec try_to_insert_new q n =
      if not_full q then (Q.insert q n; true)
      else N.beats n (bottom q)
           && match Q.odrop q with
              | None -> failwith "couldn't drop q"
              | Some b -> stubify_node q b;
                          try_to_insert_new q n

  let rec ready_to_insert q n =
      not_full q ||
      begin N.beats n (bottom q)
            && match Q.odrop q with
               | None -> failwith "couldn't drop q"
               | Some b -> stubify_node q b;
                           ready_to_insert q n
      end

   
    
don't need to update q until after update_cost
    Q.update q (c.parent.loc)


match min_child_cost n with
| None -> delete n
| Some x -> 

   let rec update q n =
      let open N in
      let cmin = min_child_cost n in
      if cmin > n.fcost
      then begin
         n.fcost <- cmin;
         if n.loc <> 0 then Q.update q n.loc;
         do_parent n (update_fcost q)
      end

   let path n =
      let rec do_node xs n =
         let p = n.N.parent in
         if p == n then xs
         else let x = (n.action,n.state) in
              do_node (x::xs) p
      in do_node [] n

   let search ~queue_size ?max_depth state =
      let root = N.make_root state in
      let q = Q.make queue_size root in
      let db = HT.create 100 in
      HT.add db state root;
      let max_depth =
         match max_depth with
         | None -> queue_size - 1
         | Some d -> if d < queue_size then d
                     else invalid_arg "max depth exceeds queue size"
      in
      let do_next_dup p =
         match L.extract_if (fun x -> not (N.indb db x)) p.dups with
         | Some (c,cs) -> HT.add db c.state c;
                          p.dups <- cs;
                          add q (N.of_stub c)
         | None -> assert (pop q == p);          (* immediately after actions run out *)
                   if N.no_fulls p then N.delete db p
      in
      let do_next_stub p =
         match p.N.stubs with
         | [] -> do_next_dup p
         | x::xs ->
         assert N.(no_fulls p || p.fcost <= min_full_child_cost p);
         p.stubs <- xs;
         let n = N.of_stub p x in
         if not (HT.mem db n.state) then begin
            HT.add db n.state n;
            fail_if (not (prepare_to_insert q n)) "do_next_stub: stub cost should equal parent cost";
            if xs = [] then assert (pop q == p);   
            N.add_child n;
            Q.insert q n;
	    update_fcost q p
         end
      in
      let rec do_next_action p next_action =
         if (N.has_fulls p) then assert N.(min_full_child_cost p >= p.fcost);
         let try_db a s =
            let add cost =
               let c = N.of_state p a s in
               HT.add db s c;
               N.push_child c;
               Q.insert q c
            in
            let cost = fcost s in
            match HT.find_opt db s with
            | None -> if ready_to_insert q cost
                      then add cost
                      else N.add_stub p {action,cost}
            | Some n -> if cost = n.fcost 
                        then N.insert_dup p {action=a;cost}
                        else if cost < n.fcost                  (* high-cost dups *)
                        then (assert (ready_to_insert q cost);
                        delete n; add cost)                     (*   get deleted  *)
         in
         match next_action () with
         | Some a -> begin match N.state_of_action n a max_depth with
                          | Some s -> try_db a s
                          | None -> do_next_action p      (* no state = infinite cost *)
                     end
         | None -> n.next_action_opt <- None;    (* last child was generated in prev. call *)
                   if N.no_stubs_or_dups n then assert (pop q == n);
                   if N.no_stubs_or_dups n
                   && N.no_fulls n then N.delete_child n
                   else update q n
      in
      let do_next_child p =
         match p.N.next_action_opt with
         | Some f -> (* print_char 'a'; *) do_next_action p f
         | None   ->    print_char 's';    do_next_stub p
      in
      let rec loop i n =
         if i = 0 || not (N.has_children n) then 
         begin
            print_newline();
            Q.print q;
            print_string "\nPress return to continue.";
            flush stdout;
            ignore (read_line());
            print_newline()
         end
         else if i mod 100 = 0 then (print_char '.'; flush stdout);
         if Prob.is_goal n.N.state then Some (path n)
         else (do_next_child n; otop q >>= loop ((i+1) mod 100))
      in
      Q.insert q root;
      loop 0 root

end


module Make = Make_with_queue (DEPQ)

