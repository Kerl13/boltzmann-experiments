let geom x =
  let rec aux xn n r =
    if r < xn then n
    else aux (x *. xn) (n + 1) (r -. xn)
  in
  aux 1. 0 (Rand.float (1. /. (1. -. x)))

let geom_cst x =
  let r = Rand.float 1. in
  log r /. log x -. 1.
  |> ceil
  |> int_of_float
