open Boltzexp

let target = 50_000_000

let bench gen =
  let open Benchtools in
  let state = Rand.get_state () in
  let res = time (fun () -> cumul_until ~target gen) in
  Rand.set_state state;
  res

let benchall l =
  let l =
    l
    |> List.map (fun (s1, s2, gen) -> (s1, s2, bench gen))
    |> List.map (fun (s1, s2, (n, t)) -> s1, s2, n, (t /. float_of_int n) *. 1e9)
    |> List.sort (fun (_, _, _, r1) (_, _, _, r2) -> Float.compare r1 r2)
  in
  let _, _, _, r0 = List.hd l in
  Format.printf "generator:bool_sampler:# of nodes:ns/node:slowdown@.";
  let print_res (s1, s2, n, r) =
    Format.printf "%s:%s:%d:%.3f:%.3fx@." s1 s2 n r (r /. r0)
  in
  List.iter print_res l

let combine rand_bool sampler =
  let sampler_name, sampler = sampler in
  let rand_bool_name, rand_bool = rand_bool in
  sampler_name, rand_bool_name, (fun () -> sampler rand_bool)

let () =
  Rand.init 41329213424289;
  let open Bintree in

  let samplers = [
    "constant space", ConstantSpace.free_size;
    (* "stack-based", StackBased.free_size; *)
    (* "gadt-stack", Gadt.free_size; *)
    (* "generic", Generic.free_size; *)
    "arbogen", Arbogen.free_size;
  ] in
  let bool_samplers = [
    "float", (fun () -> Rand.float 1. < 0.5);
    "ocaml", Random.bool;
    "bit-optimal", Rand.bool;
  ] in
  let all =
    List.map
      (fun sampler ->
        List.map (fun rand_bool -> combine rand_bool sampler) bool_samplers)
      samplers
    |> List.flatten
  in
  benchall all
