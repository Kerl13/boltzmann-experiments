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


module Bernoulli = struct
  let naive param = Rand.float 1. < param

  let bit_optimal param =
    let rec aux s x =
      if s >= param then false
      else if s +. x < param then true
      else
        let x = x /. 2. in
        if Rand.bool () then aux (s +. x) x
        else aux s x
    in
    aux 0. 1.
end
