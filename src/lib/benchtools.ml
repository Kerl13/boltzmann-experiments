let time thunk =
  let t0 = Unix.gettimeofday () in
  let x = thunk () in
  let t1 = Unix.gettimeofday () in
  x, t1 -. t0

let cumul_until ~target size_generator =
  let rec loop acc =
    if acc >= target then acc
    else loop (acc + size_generator ())
  in
  loop 0
