open Boltzexp

let benchall bencher n generators =
  let bench_one (name, generator) =
    let state = Rand.get_state () in
    let _, t = Benchtools.time (fun () -> bencher n generator) in
    Rand.set_state state;
    name, (t /. float_of_int n) *. 1e9
  in
  let results =
    generators
    |> List.map bench_one
    |> List.sort (fun x y -> Float.compare (snd x) (snd y))
  in
  let _, r0 = List.hd results in
  let print (name, r) = Format.printf "%s:%F:%.3fx@." name r (r /. r0) in
  Format.printf "name:ns/call:slowdown@.";
  List.iter print results

let cons_if b x xs =
  if b then x :: xs
  else xs

let () =
  let n = int_of_string Sys.argv.(1) in
  let param = float_of_string Sys.argv.(2) in
  let generators = [
    "naive", (fun () -> Dist.Bernoulli.naive param);
    "bit-optimal", (fun () -> Dist.Bernoulli.bit_optimal param);
  ] in
  let generators = cons_if
    (param = 0.333333333333333315)
    ("special3", Dist.Bernoulli.special3)
    generators
  in
  let generators = cons_if
    (param = 0.25)
    ("special4", Dist.Bernoulli.special4)
    generators
  in
  benchall Benchtools.gen_n_times n generators;
