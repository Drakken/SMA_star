


module Element = struct
  type t = int ref

  let make n = ref n

  let beats x y = !x > !y

  let id x = !x

  let getloc _ = 0
  let setloc _ _ = ()

  let print_row _ = ()

end

module Q = SMA_star.DEPQ.Make (Element)

module E = Element

let test_q () =
  print_endline "Running TestQ test....";
  let q = Q.make 1000 (E.make 0) in
  for n = 1 to 100 do Q.insert q (ref n) done;
  let xtop = Q.top q in
  Printf.printf "\n!xtop = %d\n" !xtop;
  match (Q.opop q) with
  | None -> print_endline "None from Q.opop"
  | Some xpop -> Printf.printf "!xpop = %d\n" !xpop;
                 assert (xtop == xpop)

;;test_q ()
