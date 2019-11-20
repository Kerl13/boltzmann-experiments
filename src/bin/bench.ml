open Boltzexp

let () =
  Rand.init 4242424242;
  Format.printf "%B %B %B@." (Rand.bool ()) (Rand.bool ()) (Rand.bool ())

