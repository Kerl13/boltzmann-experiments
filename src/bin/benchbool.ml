open Boltzexp

let gen_n n generator =
  let rec aux n =
    if n = 0 then ()
    else
      let _ = generator () in
      aux (n - 1)
  in
  aux n

let add_n n generator =
  let rec aux acc n =
    if n = 0 then acc
    else if generator () then aux (acc + 1) (n - 1)
    else aux acc (n - 1)
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
  Format.printf "name:ns/bit:slowdown@.";
  List.iter print results

let () =
  let n = 50_000_000 in
  let generators = [
    "bit-optimal", Rand.bool;
    "ocaml", Random.bool;
    "float", (fun () -> Rand.float 1. < 0.5);
  ] in
  Format.printf "=== generation-only ===@.";
  benchall gen_n n generators;
  Format.printf "\n=== generate and add ===@.";
  benchall add_n n generators
