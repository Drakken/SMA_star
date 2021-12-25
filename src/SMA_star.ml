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
   type dup  = { dcost: int; action: Prob.action; dstate: Prob.state }

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
    mutable  dups: dup  list;
   }

   let getloc n   = n.loc
   let setloc n i = n.loc <- i

   let to_stub n = recycle_id n.id; { scost = n.fcost; action = n.action }
(*
   let to_dup  n = recycle_id n.id; { dcost = n.fcost; action = n.action; dstate = n.state }
*)
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
         dups  = [];
       }
      in root
 
   let do_parent n f =
      let p = n.parent in
      if p != n then f p        (* the root node is its own parent *)

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
      get_action_opt = Some (Prob.make_action_generator state);
    }

   let next_state p a = Prob.next_state p.state a

   let gcost p a = p.gcost + Prob.delta_g_cost_of_action p.state a

   let of_stub p {action;scost} =
      let state = next_state p action in
                                        make_child p action  state scost (gcost p action)
   let of_dup p {action;dstate;dcost} = make_child p action dstate dcost (gcost p action)

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

   let[@inline] no_fulls n = n.fulls = []
   let[@inline] no_stubs n = n.stubs = []
   let[@inline] no_dups  n = n.dups  = []
   let[@inline] no_nexts n = n.get_action_opt = None
(*
   let[@inline] no_stubs_or_dups n = no_stubs n && no_dups n

   let[@inline] has_fulls n = n.fulls <> []
   let[@inline] has_stubs n = n.stubs <> []
   let[@inline] has_dups  n = n.dups  <> []
   let[@inline] has_nexts n = n.get_action_opt <> None
*)
   let[@inline] has_fulls_only  n =  no_stubs n &&  no_nexts n &&  no_dups n
(*
   let[@inline] has_no_children n =  no_stubs n &&  no_nexts n &&  no_dups n &&  no_fulls n

   let[@inline]    has_children n = has_stubs n || has_nexts n || has_dups n || has_fulls n
*)
   let insert_by_cost cost_of xs x =
      let rec ins rev_ys = function
       | y::ys as xs -> if cost_of x <= cost_of y
                        then L.rev_append rev_ys (x::xs)
                        else ins (y :: rev_ys) ys
       | [] -> L.rev_append rev_ys [x]
      in
      ins [] xs

   let insert_stub_by_cost = insert_by_cost (fun s -> s.scost)
   let insert_dup_by_cost  = insert_by_cost (fun d -> d.dcost)

   let min_stub_cost = function [] -> None | x::_ -> Some x.scost
   let  min_dup_cost = function [] -> None | x::_ -> Some x.dcost
     
   let min_full_cost n = 
      match n.fulls with
       | [] -> None
       | ns -> Some (L.min_of (fun n -> n.fcost) ns)

   let min_child_cost_opt n =
    [ min_full_cost n;
      min_stub_cost n.stubs;  
      min_dup_cost  n.dups ] |> L.concat_map Option.to_list |> L.min_opt

   let[@inline] delete_child c =
      let p = c.parent in 
      p.fulls <- L.remove_object c p.fulls

   let[@inline] push_child c = let p = c.parent in p.fulls <- c :: p.fulls

   let[@inline] add_dup  p d = p.dups  <- insert_dup_by_cost  p.dups d
   let[@inline] add_stub p s = p.stubs <- insert_stub_by_cost p.stubs s

   let insert_stub c = add_stub c.parent (to_stub c)

   let[@inline] insert_dup p d = p.dups <- insert_dup_by_cost p.dups d

   let    beats   a   b = a.fcost < b.fcost || (a.fcost = b.fcost && a.depth > b.depth)
   let cd_beats (c,d) b =    c    < b.fcost || (   c    = b.fcost &&   d     > b.depth)

   let to_strings n =
      let    line0 = sprintf "id%d p%d d%d" n.id n.parent.id n.depth
      in let line1 = sprintf  "a%s g%d f%d" (Prob.string_of_action n.action) n.gcost n.fcost
      in
      let line2 = sprintf "c%d s%d n%c"
         (L.length n.fulls)
         (L.length n.stubs) (if n.get_action_opt = None then 'n' else 'y')     
      in
      let state_lines = Prob.strings_of_state n.state
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
    val  pop:    t -> element
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
         delete_child c;
         insert_stub c;
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

   let rec ready_to_insert_cd q cd =
      not_full q ||
      begin N.cd_beats cd (bottom q)
            && match Q.odrop q with
                | None -> failwith "couldn't drop q"
                | Some n -> stubify_node q n;
                            ready_to_insert_cd q cd
      end

   let path n =
      let rec do_node acc n =
         let p = n.N.parent in
         if p == n then acc
         else let x = (n.action,n.state) in
              do_node (x::acc) p
      in do_node [] n

   let search ~queue_size ?max_depth state =
      let root = N.make_root state in
      let q = Q.make queue_size root in
      let ready_to_insert p cost = ready_to_insert_cd q (cost,(p.N.depth+1)) in
      let db = HT.create queue_size in
      let module DB = struct
         let mem s = HT.mem db s
         let add s n = HT.add db s n
         let remove s = HT.remove db s
         let find_opt s = HT.find_opt db s
      end in
      DB.add state root;
      let max_depth =
         match max_depth with
          | None -> queue_size - 1
          | Some d -> if d < queue_size then d
                      else invalid_arg "max depth exceeds queue size"
      in
      let module Q = struct
         open N
         let update n = Q.update q n.loc
         let pop n = assert (Q.pop q == n)
(*         let eject n = Q.eject q n
*)
         let insert n = Q.insert q n
      end in
(*
         let rec prune n =
            L.iter prune n.fulls;
            assert (DB.mem n.state);
            DB.remove n.state;
            Q.eject n;
            delete_child n;
            recycle_id n.id;
            do_parent n update
      in
*)
      let rec
         update n =             (* after all successors have been generated *)
            let open N in
            match min_child_cost_opt n with
             | None -> if n.loc = 0 then delete n
                       else Q.update n
             | Some cmin ->
               if cmin > n.fcost then begin
                  n.fcost <- cmin;
                  if n.loc <> 0 then Q.update n;
                  do_parent n update
               end
      and 
         delete n =
            let open N in
            assert (n.loc = 0); 
            delete_child n;
            recycle_id n.id;
            assert (DB.mem n.state);
            DB.remove n.state;
            do_parent n update
      in
      let pop n = Q.pop n; if N.no_fulls n then delete n
      in
      let rec do_next_stub p =
         let open N in
         match p.stubs with
          | [] -> pop p          (* immediately after actions run out *)
          | x::xs ->
              let s = state_of_action p x.action max_depth in
              if DB.mem s (* DUPSWAP what if the stub is better? *)
              then begin
                 p.stubs <- xs;
                 insert_dup p {dcost=x.scost;action=x.action;dstate=s};
                 do_next_stub p
              end
              else if (not (ready_to_insert p x.scost))
              then pop p
              else begin
                 let n = of_stub p x in
                 DB.add n.state n;
                 push_child n;
                 Q.insert n;
	         update p
              end
      in
      let do_next_dup p =
         match L.extract_if (fun d -> not (DB.mem d.N.dstate)) p.N.dups with
          | None -> do_next_stub p
          | Some (d,ds) -> if not (ready_to_insert p d.N.dcost)
                           then do_next_stub p
                           else begin
                              let n = N.of_dup p d in
                              DB.add d.N.dstate n;
                              p.dups <- ds;
                              Q.insert n;
                              update p
                           end
      in
      let rec do_next_action p get_action =
         let try_db a s =
            let add () =
               let c = N.of_state p a s in
               DB.add s c;
               N.push_child c;
               Q.insert c
            in
            let cost = N.fcost_of_action p a in
            match DB.find_opt s with
             | None -> if ready_to_insert p cost
                       then add ()
                       else N.add_stub p {action=a;scost=cost}
             | Some _ -> N.add_dup p {action=a;dcost=cost;dstate=s}
(*
                       if cost >= n.fcost
                         then N.add_dup p {action=a;dcost=cost;dstate=s}
                         else (assert (ready_to_insert p cost);
                               prune n; add ())
*)
         in
         let try_state a =
            match N.state_of_action_opt p a max_depth with
             | Some s -> try_db a s
             | None -> do_next_action p get_action      (* no state = infinite cost *)
         in
         match get_action () with
          | Some a -> try_state a
          | None -> p.get_action_opt <- None;    (* last child was generated in prev. call *)
                    update p
      in
      let do_next_child p =
         match p.N.get_action_opt with Some f -> do_next_action p f
                                     | None   -> do_next_dup p
      in
      let rec loop i n =
         if i = 0 then pause "\nPress return to continue.";
         if Prob.is_goal n.N.state then Some (path n)
         else begin
            do_next_child n;
            if i mod 100 = 99 then (print_char '.'; flush stdout);
            otop q >>= loop ((i+1) mod 1000)
         end
      in
      Q.insert root;
      loop 0 root

end


module Make = Make_with_queue (DEPQ)

