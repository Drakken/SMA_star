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

let pause msg =
   print_string msg;
   flush stdout;
   ignore (read_line())


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
   val strings_of_state: state  -> string list

end


module Node (Prob: Typeof_Problem) = struct

   type stub = { scost: int; action: Prob.action }

   type t = {
    id: int;
    mutable loc: int;
    parent: t;
    depth: int;
    action: Prob.action;
    state:  Prob.state;
    gcost: int;
    mutable fcost: int;
    mutable get_action_opt: Prob.action Generator.t option;
    mutable fulls: t list;
    mutable stubs: stub list;
   }

   let[@inline] getloc n   = n.loc
   let[@inline] setloc n i = n.loc <- i

   let to_stub n = recycle_id n.id; { scost = n.fcost; action = n.action }

   let make_root state =
      let rec root = {
         id = new_id();
         loc = 0;
         parent = root;
         depth = 0;
         action = Prob.trivial_action;
         state;
         get_action_opt = Some (Prob.make_action_generator state);
         gcost = 0;
         fcost = Prob.h_cost_to_goal state;
         fulls = [];
         stubs = [];
       }
      in root
 
   let do_parent n f =
      let p = n.parent in      (* the root node is its own parent *)
      if p != n then f p       (*       (don't do it twice)       *)

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
      get_action_opt = Some (Prob.make_action_generator state);
    }

   let next_state p a = Prob.next_state p.state a

   let gcost p a = p.gcost + Prob.delta_g_cost_of_action p.state a

   let of_stub p {action;scost} =
      make_child p action (next_state p action) scost (gcost p action)

   let state_of_action p a max_depth = 
      let s = next_state p a in
      if Prob.is_goal s || p.depth < max_depth - 1
      then s
      else invalid_arg "state_of_action: no state"

   let state_of_action_opt p a max_depth = 
      let s = next_state p a in
      if Prob.is_goal s || p.depth < max_depth - 1
      then Some s
      else None

   let fcost_of_action p a = max p.fcost (gcost p a + Prob.h_cost_to_goal (next_state p a))

   let of_state p a s = 
      let gcost = gcost p a in
      let fcost = max p.fcost (gcost + Prob.h_cost_to_goal s)
      in make_child p a s fcost gcost

   let[@inline] no_actions n = n.get_action_opt = None

   let[@inline] no_fulls n = n.fulls = []
   let[@inline] no_stubs n = n.stubs = []

   let[@inline] has_fulls_only n = no_stubs n && no_actions n

   let insert_stub_by_cost xs x =
      let rec ins revs = function
       | [] -> L.rev_append revs [x]
       | y::ys as yys -> if x.scost <= y.scost
                         then L.rev_append revs (x::yys)
                         else ins (y :: revs) ys
      in ins [] xs

   let min_stub_cost = function [] -> None | x::_ -> Some x.scost
     
   let min_full_cost n = 
      match n.fulls with
       | [] -> None
       | ns -> Some (L.min_of (fun n -> n.fcost) ns)

   let min_child_cost_opt n =
    [ min_full_cost n;
      min_stub_cost n.stubs] |> L.concat_map Option.to_list |> L.min_opt

   let[@inline] push_full c = let p = c.parent in p.fulls <- c :: p.fulls

   let[@inline] add_stub p s = p.stubs <- insert_stub_by_cost p.stubs s

   let insert_stub c = add_stub c.parent (to_stub c)

   let    beats   a   b = a.fcost < b.fcost || (a.fcost = b.fcost && a.depth > b.depth)
   let cd_beats (c,d) b =    c    < b.fcost || (   c    = b.fcost &&   d     > b.depth)

   let to_strings n =
      let    line0 = sprintf "i%dp%dd%d" n.id n.parent.id n.depth
      in let line1 = sprintf  "a%s g%d f%d" (Prob.string_of_action n.action) n.gcost n.fcost
      in
      let line2 = "c:" ^ L.(fold_left (fun s1 s2 -> s1^" "^s2) "" (map (fun c -> string_of_int c.id) n.fulls))
      in
      let line3 = sprintf "s%d n%c L%d"
         (L.length n.stubs) (if n.get_action_opt = None then 'n' else 'y') n.loc  
      in
      let state_lines = Prob.strings_of_state n.state
      in
      line0::line1::line2::line3::state_lines

   let print n = L.iter print_endline (to_strings n)

   let[@inline] delete_full c =
      let p = c.parent in p.fulls <- L.remove_object c p.fulls

end


module type Typeof_Queue = sig

  module Make (E: Element.T): sig

    type element = E.t
    type t

    val make: int -> element -> t

    val size: t -> int

    val insert: t -> element -> unit

    val  top:   t -> element
    val  pop:   t -> element
    val drop:   t -> element
    val bottom: t -> element

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
         queue_size: int ->
         ?max_depth: int ->
         ?printing: bool ->
         Prob.state -> (Prob.action * Prob.state) list option
    end


module Make_with_queue (Queue: Typeof_Queue)
                       (Prob:  Typeof_Problem)
      = struct

   module N = Node (Prob)
   module Q = Queue.Make (N)

   let path n =
      let rec do_node acc n =
         let p = n.N.parent in
         if p == n then acc
         else let x = (n.action,n.state) in
              do_node (x::acc) p
      in do_node [] n


   let search ~queue_size
              ?(max_depth = queue_size - 1)
              ?(printing = false)
              initial_state
      =
      if max_depth >= queue_size
         then invalid_arg "search: max_depth must be less than queue_size"
      else
      let module DB = struct
         let db = HT.create queue_size
         let mem s = HT.mem db s
         let add s n = HT.add db s n
         let remove s = HT.remove db s
         let find_opt s = HT.find_opt db s
      end
      in let root = N.make_root initial_state
      in
      let module Q = struct
         open N
         let q = Q.make queue_size root
         let print () = Q.print q
         let update n = Q.update q n.loc
         let top () = Q.top q
         let pop n = assert (Q.pop q == n)
         let drop () = Q.drop q
         let bottom () = Q.bottom q
         let not_full () = not (Q.is_full q)
         let insert n = Q.insert q n
(*
         let eject n = Q.eject q n        (* DUPSWAP *)
         let rec prune n =
            L.iter prune n.fulls;
            assert (DB.mem n.state);
            DB.remove n.state;
            Q.eject n;
            delete_full n;
            recycle_id n.id;
            do_parent n update
*)
      end
      in
      let delete_child_node c =
         let open N in
         assert (c.loc = 0);
         delete_full c;
         assert (DB.mem c.state);
         DB.remove      c.state;
         recycle_id c.id
      in
      let rec
         stubify_leaf_node c p = 
            let open N in
            let p_had_fulls_only = has_fulls_only p in          (* not in the queue *)
            if p_had_fulls_only && p.loc <> 0
            then (N.print p; N.print c; failwith "stubify_leaf_node: p.loc <> 0");
            delete_child_node c;
            insert_stub c;
            if p_had_fulls_only then assert (try_to_insert_old p)
      and
         try_to_stubify n = N.(if no_fulls n then do_parent n (stubify_leaf_node n))
      and
         drop_and_try_again n = try_to_stubify (Q.drop()); try_to_insert_old n
      and
         try_to_insert_old n =
            if Q.not_full() then (Q.insert n; true)
            else if N.beats n (Q.bottom()) then drop_and_try_again n
            else (try_to_stubify n; false)
      in
      let ready_to_insert_new p cost = 
         let cd = (cost,(p.N.depth+1)) in
         let rec ready() =
            Q.not_full() || (N.cd_beats cd (Q.bottom())
                             && (try_to_stubify (Q.drop()); ready()))
         in ready() 
      in
      let rec
         update n =             (* after all successors have been generated *)
            let open N in
            match min_child_cost_opt n with
             | None -> if n.loc <> 0 then Q.update n
                       else if no_actions n then delete n
             | Some cmin ->
                  if cmin > n.fcost then begin
                     n.fcost <- cmin;
                     if n.loc <> 0 then Q.update n;
                     do_parent n update
                  end
      and 
         delete n = delete_child_node n; N.do_parent n update
      in
      let pop n = Q.pop n; if N.no_fulls n then delete n
      in
      let insert_child c =
         DB.add c.N.state c;
         N.push_full c;
         Q.insert c
      in
      let rec do_next_stub p =
         let open N in
         match p.stubs with
          | [] -> pop p          (* immediately after actions run out *)
          | x::xs ->
              let s = state_of_action p x.action max_depth in
              if DB.mem s (* DUPSWAP what if the stub is better? *)
              then (p.stubs <- xs; do_next_stub p)
              else if ready_to_insert_new p x.scost
              then insert_child (of_stub p x)
              else pop p
      in
      let rec do_next_action p get_action =
         let try_db a s =
            let cost = N.fcost_of_action p a in
            match DB.find_opt s with
             | Some _ -> ()    (* DUPSWAP *)
             | None ->
                  if ready_to_insert_new p cost
                  then insert_child (N.of_state p a s)
                  else N.add_stub p {action=a;scost=cost}
         in
         let try_state a =
            match N.state_of_action_opt p a max_depth with
             | Some s -> try_db a s
             | None -> do_next_action p get_action      (* no state = infinite cost *)
         in
         match get_action () with
          | Some a -> try_state a
          | None -> p.get_action_opt <- None;    (* last child was generated in prev. call *)
                    if N.has_fulls_only p then Q.pop p;
                    update p
      in
      let do_next_child p =
         match p.N.get_action_opt with Some f -> do_next_action p f
                                     | None   -> do_next_stub p
      in
      let rec loop i n =
         if i = 0 && printing
         then begin
            print_endline "\nThe queue:";
            Q.print();
            pause "\nPress return to continue."
         end;
         if Prob.is_goal n.N.state then Some (path n)
         else begin
            do_next_child n;
            if printing && i mod 100 = 99 then (print_char '.'; flush stdout);
            loop ((i+1) mod 10000) (Q.top())
         end
      in
      DB.add initial_state root;
      Q.insert root;
      try loop 0 root with _ -> None

end


module Make = Make_with_queue (DEPQ)

