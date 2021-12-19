(*
 * array-based double-ended priority queue (DEP queue)
 * heap implementation based on Cormen et. al., Introduction to Algorithms
 * copyright (c) 2021 Daniel S. Bensen
 *)

open Printf
open Utils

module A = Array

exception Overflow
exception Underflow

let underflow () = raise Underflow
let  overflow () = raise Overflow

let[@inline] n_parent n = n/2    
let[@inline] n_left   n = 2*n    
let[@inline] n_right  n = 2*n + 1

let node_width = 18

module Make (E: Element.T) = struct

  type element = E.t

  module Heap = struct

    type t = { array: element array; sign: int; mutable size: int; beats: element -> element -> bool }

    let make max x sign beats = { array = Array.make max x; sign; size = 0; beats }

    let max h = Array.length h.array

    let[@inline] incr h = h.size <- h.size + 1
    let[@inline] decr h = h.size <- h.size - 1

    let clear h = h.size <- 0

    let[@inline] ( .:()   ) h n   = h.array.(n-1)
    let[@inline] ( .:()<- ) h n x = h.array.(n-1) <- x

    let[@inline] setloc x h n = E.setloc x (h.sign * n)

    let[@inline] setloc0 x = E.setloc x 0

    let verify h n msg =
      let s = h.size in
      let parent_is_ok = n = 1              || not (h.beats h.:(    n    ) h.:(n_parent n))
      and   left_is_ok = n > n_parent   s   || not (h.beats h.:(n_left  n) h.:(    n     ))
      and  right_is_ok = n > n_parent (s-1) || not (h.beats h.:(n_right n) h.:(    n     ))
      and    loc_is_ok = E.getloc h.:(n) = h.sign * n
      in verify parent_is_ok (msg ^ ": parent")
      ;  verify   left_is_ok (msg ^ ": left")
      ;  verify  right_is_ok (msg ^ ": right")
      ;  verify    loc_is_ok (msg ^ ": loc")

    let[@inline] add_new x h n = h.:(n)<- x; setloc x h n

    let[@inline] move h n1 n2 = add_new h.:(n1) h n2

    let[@inline] hop_raw src dest n =
      dest.:(n)<- src.:(n);
      setloc dest.:(n) dest n

    let[@inline] swap2 h1 n1 h2 n2 =
      let x1 = h1.:(n1)
      and x2 = h2.:(n2) in
      h1.:(n1)<- x2; setloc x2 h1 n1;
      h2.:(n2)<- x1; setloc x1 h2 n2

    let[@inline] swap h n1 n2 = swap2 h n1 h n2

    let rec heapify_n h n =
      let nl = n_left  n
      and nr = n_right n
      in let nbig = if nl <= h.size && h.beats h.:(nl) h.:( n )  then nl else n
      in let nbig = if nr <= h.size && h.beats h.:(nr) h.:(nbig) then nr else nbig
      in
      if nbig = n then n
      else (swap h n nbig; heapify_n h nbig)

    let heapify h = heapify_n h 1

    let pop h =
      let n = h.size in
      if n = 0 then underflow()
      else let x = h.:(1) in
      decr h;
      setloc0 x;
      if n = 1 then x,0
      else let () = move h n 1 in
      if n = 2 then (x,1)
      else (x, heapify h)

    let floatt h n =
      let rec do_n n =
        if n = 1 then 1
        else
        let np = n_parent n in
        if h.beats h.:(n) h.:(np)
        then (swap h n np; do_n np)
        else n
      in do_n n

    let insert h x =
      incr h;
      let n = h.size in
      if n > max h then (decr h; overflow())
      else (add_new x h n; verify h (floatt h n) "H.insert")

    let hop a b =
      assert (a.size - b.size = 1);
      let n = a.size in
      let x = a.:(n) in
      if n > 1 then assert (not (b.beats x b.:(n_parent n)));
      decr a;
      incr b;
      hop_raw a b n

    let update h n =
      if n > 1 then assert (not (h.beats h.:(n) h.:(n_parent n)));
      heapify_n h n

    let print_row h nrow =
      let nmin = 2**nrow in
      let ntop = min (2*nmin) (h.size + 1) in
      let num_nodes = ntop - nmin in
      let nodes = L.(map ((.:()) h) (init num_nodes ((+) nmin)))
      in Ascii_art.print_row node_width E.to_strings nodes

    let print h f =
      if h.size > 0 then begin
        printf "size = %d\n" h.size;
        let num_rows = 1 + intlog2 h.size in
        L.iter (print_row h) (f (L.init num_rows Fun.id))
      end

  end

  module H = Heap

  let ( .:() ) = H.( .:() )

  type t = { hi: H.t; lo: H.t }

  let print q =
    H.print q.hi Fun.id;
    H.print q.lo L.rev

  let make max x =
    let n = (max+1)/2 in
    { hi = H.make n x   1            E.beats;
      lo = H.make n x (-1) (Fun.flip E.beats) }

  let[@inline] max  q = 2 * (A.length q.lo.H.array)
  let[@inline] size q = q.lo.H.size + q.hi.H.size

  let[@inline]  is_empty q = size q = 0
  let[@inline]  is_full  q = size q = max q
  let[@inline] not_full  q = size q < max q

  let[@inline] hdiff q = q.hi.H.size - q.lo.H.size

  let[@inline] hhi q = q.hi
  let[@inline] hlo q = q.lo

  let[@inline] tob fh q =           (* top or bottom :) *)
    let size = size q
    in   if size > 1 then (fh q).:(1)
    else if size = 0 then underflow()
    else if hdiff q = 1
    then q.hi.:(1)
    else q.lo.:(1)

  let[@inline] otob fh q =
    let size = size q
    in   if size > 1 then Some (fh q).:(1)
    else if size = 0 then None
    else if hdiff q = 1
    then Some q.hi.:(1)
    else Some q.lo.:(1)

  (* let  top q =  tob hhi q *)
  let otop q = otob hhi q

  let  bottom q =  tob hlo q
  let obottom q = otob hlo q

  let clear q =
    H.clear q.hi;
    H.clear q.lo

  let insert_raw q xnew =
    let insert01 h0 h1 =
      if h1.H.beats xnew h1.:(1)
      then begin
        H.hop h1 h0;
        H.insert h1 xnew;
        assert (h1.H.beats h1.:(1) h0.:(1));
        assert (h0.H.beats h0.:(1) h1.:(1))
      end
      else H.insert h0 xnew
    in
    match (q.lo.H.size, q.hi.H.size) with
    | 0,0 -> H.insert q.hi xnew
    | 0,1 -> insert01 q.lo q.hi
    | 1,0 -> insert01 q.hi q.lo
    | slo,shi when slo = shi ->
         if q.hi.H.beats xnew q.hi.:(n_parent (slo+1))
         then H.insert q.hi xnew
         else H.insert q.lo xnew
    | slo,shi ->
         let (big,med) = if shi > slo
                     then (q.hi,q.lo)
                     else (q.lo,q.hi)
         in let n = big.H.size
         in let xmid = big.:(n)
         in if not (big.H.beats xnew xmid)
         then H.insert med xnew
         else H.(decr big;
                 insert big xnew;
                 insert med xmid)

  let  insert q x = if not_full q then       insert_raw q x  else overflow()
  let oinsert q x = if not_full q then Some (insert_raw q x) else None
(*
  let maybe_swap a na b =
    let swap nb =
      if a.H.beats b.:(nb) a.:(na)
      then let () = H.swap2 a na b nb
      in H.verify b (H.floatt b nb) "maybe_swap"
    in
      let sa = a.H.size
      and sb = b.H.size in
      let pb = n_parent sb in
      if na > pb then swap na
      else if sa < sb && na = pb       (* find the node in b (if any) that *)
      then if is_odd sa                (*  a.:(na) has to be compared to   *)
      then swap (n_left  na)           (*    see ascii art for details     *)
      else swap (n_right na)
*)
  let maybe_swap a na b =
    let swap nb =
      if a.H.beats b.:(nb) a.:(na)
      then let () = H.swap2 a na b nb
      in H.verify b (H.floatt b nb) "maybe_swap"
    in
    match (a.H.size, b.H.size) with
    | 0,0 -> invalid_arg "empty queue in maybe_swap"
    | 0,1 -> ()
    | 1,0 -> ()
    | sa,sb ->
        if sa = sb then swap na
        else if sb - sa = 1 then        (* a is the upper heap in the ascii art *)
        begin
          let pb = n_parent sb in
          if na > pb then swap na
          else if na = pb                  (* find the node in b (if any) that *)
          then if is_odd sa                (*  a.:(na) has to be compared to   *)
          then swap (n_left  na)           (*    see ascii art for details     *)
          else swap (n_right na)
        end
        else if sa - sb = 1 then        (* a is the lower heap in the ascii art *)
        begin
          if na > (n_parent sa) then swap na
          else if na = sa   
          then swap (n_parent na)
        end
        else invalid_arg (sprintf "Q.maybe_swap: a.size - b.size = %d" (sa - sb))

  let pop_n_swap a b =
    let y = a.:(1) in
    let (x,na) = H.pop a in
    assert (x == y);
    let () = maybe_swap a na b
    in x

  (* pop & drop: if there's an extra element in the *)
  (*  other heap, move it to the heap being popped  *)

  let pop_raw q =
    let open H in
    if hdiff q = -1 then
    begin
      let n = q.lo.size in
      if n > 1 then assert (not (q.hi.beats q.lo.:(n) q.hi.:(n_parent n)));
      H.hop q.lo q.hi
    end;
    pop_n_swap q.hi q.lo  


  let drop_raw q =
    let open H in
    if hdiff q = 1 then
    begin
      let n = q.hi.size in
      if n > 1 then assert (not (q.lo.beats q.hi.:(n) q.lo.:(n_parent n)));
      H.hop q.hi q.lo
    end;
    pop_n_swap q.lo q.hi


  let   pop q = if size q > 0 then  pop_raw q  else underflow()
  let  drop q = if size q > 0 then drop_raw q  else underflow()

  let top q =
    let size = size q
    in   if size > 1 then q.hi.:(1)
    else if size = 0 then underflow()
    else if hdiff q = 1
    then q.hi.:(1)
    else q.lo.:(1)

  let opop q =
    if size q = 0 then None
    else 
    let x = top q in
    let y = pop_raw q in
    if x == y then Some y
    else failwith "opop: x != y"

  let odrop q = if size q > 0 then Some (drop_raw q) else None

  let update q loc =
    if      loc = 0 then invalid_arg "update: loc = 0"
    else if loc < 0 then ignore (H.floatt q.lo (-loc))
    else let n1 = H.update q.hi loc
    in maybe_swap q.hi n1 q.lo

  let element_of_loc q n =
    if      n>0 then q.hi.:(n)
    else if n<0 then q.lo.:(-n)
    else invalid_arg "element_of_loc: loc = 0"

end

