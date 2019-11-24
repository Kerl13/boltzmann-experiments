open Boltzexp

let add_n n generator =
  let rec aux acc n =
    if n = 0 then acc
    else 
      let x = generator () in
      aux (acc + x) (n - 1)
  in
  aux 0 n

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

let () =
  let n = int_of_string Sys.argv.(1) in
  let param = float_of_string Sys.argv.(2) in
  let generators = [
    "log", (fun () -> Dist.geom param);
    "recursive", (fun () -> Dist.geom_cst param);
  ] in
  Format.printf "=== generation-only ===@.";
  benchall Benchtools.gen_n_times n generators;
  Format.printf "\n=== generate and add ===@.";
  benchall add_n n generators

