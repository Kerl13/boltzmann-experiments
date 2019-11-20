open Boltzexp

let target = 200_000_000

let bench gen =
  let open Benchtools in
  let state = Rand.get_state () in
  let res = time (fun () -> cumul_until ~target gen) in
  Rand.set_state state;
  res

let benchall l =
  let l = List.map (fun (name, gen) -> (name, bench gen)) l in
  let compare (_, (_, t)) (_, (_, t')) = Float.compare t t' in
  let l = List.sort compare l in
  let _, (n, t0) = List.hd l in
  let n = float_of_int n in
  Format.printf "name:runtime:slowdown:ns/node@.";
  let print_res name t =
    Format.printf "%s:%Fs:%.3fx:%.3f@."
      name t (t0 /. t) ((t /. n) *. 1e9)
  in
  List.iter (fun (name, (_, t)) -> print_res name t) l

let () =
  Rand.init 41329213424289;
  let open Bintree in

  let l = [
    "constant space", ConstantSpace.free_size;
    "stack-based", StackBased.free_size;
    "gadt-stack", Gadt.free_size;
    "generic", Generic.free_size;
    "arbogen", Arbogen.free_size;
  ] in
  let ll =
    List.fold_right
      (fun (name, f) acc ->
        (name, fun () -> f Rand.bool)
        :: (name ^ "'", fun () -> f Random.bool)
        :: acc)
      l
      []
  in
  benchall ll
