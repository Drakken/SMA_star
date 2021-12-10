(*
 * array-based double-ended priority queue (DEP queue)
 * heap implementation based on Cormen et. al., Introduction to Algorithms
 * copyright (c) 2021 Daniel S. Bensen
 *)

open Printf

module A = Array

(* let[@inline] is_even n = ( n  = 2*(n/2)) *)
let[@inline] is_odd  n = (n-1 = 2*(n/2))

exception Overflow
exception Underflow

let underflow () = raise Underflow
let  overflow () = raise Overflow

let[@inline] n_parent n = n/2    
let[@inline] n_left   n = 2*n    
let[@inline] n_right  n = 2*n + 1


module Make (E: Element.T) = struct

  type element = E.t

  module Heap = struct

    type t = { array: element array; sign: int; mutable size: int; beats: element -> element -> bool }

    let make max x sign beats = { array = Array.make max x; sign; size = 0; beats }

    let max h = Array.length h.array

    let print_ids { array; size; _ } =
      printf "(%d): " size;
      for n = 0 to size do printf " %3d " (E.id array.(n)) done

    let[@inline] incr h = h.size <- h.size + 1
    let[@inline] decr h = h.size <- h.size - 1

    let clear h = h.size <- 0

    let[@inline] ( .:()   ) h n   = h.array.(n-1)
    let[@inline] ( .:()<- ) h n x = h.array.(n-1) <- x

    let[@inline] setloc x h n = E.setloc x (h.sign * n)

    let[@inline] setloc0 x = E.setloc x 0

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
        if n > 1 then
        let np = n_parent n in
        if h.beats h.:(n) h.:(np)
        then (swap h n np; do_n np)
      in do_n n

    let insert h x =
      incr h;
      let n = h.size in
      if n > max h then (decr h; overflow())
      else (add_new x h n; floatt h n)

    let hop src dest =
      assert (src.size - dest.size = 1);
      let n = src.size in
      let x = src.:(n) in
      assert (not (dest.beats x dest.:(n_parent n)));
      decr src;
      incr dest;
      hop_raw src dest n

    let update h n =
      if n = 1 then heapify h
      else let () = assert (not (h.beats h.:(n) h.:(n_parent n)))
      in heapify_n h n

  end

  module H = Heap

  let ( .:() ) = H.( .:() )

  type t = { hi: H.t; lo: H.t }

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

  let print_ids q =
    print_string "\nhi"; H.print_ids q.hi;
    print_string "\nlo"; H.print_ids q.lo; print_newline ()

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
    let insert01 a b =
      if not (b.H.beats xnew b.:(1))
      then H.insert a xnew
      else (H.hop b a; H.insert b xnew)
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

  let maybe_swap a na b =
    let swap nb =
      if a.H.beats b.:(nb) a.:(na)
      then let () = H.swap2 a na b nb
      in H.floatt b nb
    in
      let sa = a.H.size
      and sb = b.H.size in
      let pb = n_parent sb in          (*  find the node in b (if any)  *)
      if na > pb then swap na          (* that na has to be compared to *)
      else if sa < sb && na = pb
      then if is_odd sa
      then swap (n_left  na)
      else swap (n_right na)

  let pop_n_swap a b =
    let y = a.:(1) in
    let (x,na) = H.pop a in
    assert (x == y);
    let () = maybe_swap a na b
    in x

  (* pop & drop: if there's an extra element in the *)
  (*  other heap, move it to the heap being popped  *)

  let  pop_raw q = if hdiff q = -1 then H.hop q.lo q.hi; pop_n_swap q.hi q.lo  
  let drop_raw q = if hdiff q =  1 then H.hop q.hi q.lo; pop_n_swap q.lo q.hi

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
    else
    begin
      print_ids q;
      printf "\nsize q = %d. x.id = %d. y.id = %d\n" (size q) (E.id x) (E.id y);
      failwith "opop: x != y"
    end


  let odrop q = if size q > 0 then Some (drop_raw q) else None

  let update q loc =
    if loc <> 0 then
    let h1,h2 = if loc > 0 then q.hi,q.lo
                           else q.lo,q.hi
    in let n1 = H.update h1 (abs loc)
    in maybe_swap h1 n1 h2

end

