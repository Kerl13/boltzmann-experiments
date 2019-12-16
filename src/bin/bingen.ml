open Boltzexp

let size_min = 500_000
let size_max = 1_000_000
let nb_generations = 5

let rec search free_size =
  let state = Rand.get_state () in
  let size = free_size () in
  if size < size_min || size > size_max then
    search free_size
  else
    (size, state)

let once free_size free_gen =
  let size, state = search free_size in
  Rand.set_state state;
  let _, t = Benchtools.time free_gen in
  size, t

let do_n_sum thunk =
  let rec loop size t n =
    if n = 0 then (size, t)
    else
      let s, t' = thunk () in
      loop (s + size) (t +. t') (n - 1)
  in
  loop 0 0.

let bench (free_size, free_gen) =
  let state = Rand.get_state () in
  let res = do_n_sum (fun () -> once free_size free_gen) nb_generations in
  Rand.set_state state;
  res

let benchall l =
  let l =
    l
    |> List.map (fun (name, g) -> (name, bench g))
    |> List.map (fun (name, (size, t)) -> name, size, (t /. float_of_int size) *. 1e9)
    |> List.sort (fun (_, _, r1) (_, _, r2) -> Float.compare r1 r2)
  in
  let _, _, r0 = List.hd l in
  Format.printf "generator:# of nodes:ns/node:slowdown@.";
  let print_res (name, size, r) =
    Format.printf "%s:%d:%.3f:%.3fx@." name size r (r /. r0)
  in
  List.iter print_res l

module BinTree = struct
  type t = Leaf | Node of t * t
end

module AdHoc = struct
  let free_size =
    let rec aux size todo =
      if todo = 0 then size
      else if Rand.bool () then aux size (todo - 1)
      else aux (size + 1) (todo + 1)
    in
    fun () -> aux 0 1

  type task = Build | Gen

  let unwrap = function
    | [x] -> x
    | _ -> invalid_arg "unwrap"

  let pop2 = function
    | x :: y :: xs -> x, y, xs
    | _ -> invalid_arg "pop2"

  let free_gen =
    let rec aux generated = function
      | [] -> unwrap generated
      | Gen :: next ->
        if Rand.bool () (* XXX. *)
        then aux (BinTree.Leaf :: generated) next
        else aux generated (Gen :: Gen :: Build :: next)
      | Build :: next ->
        let x, y, generated = pop2 generated in
        aux (BinTree.Node (x, y) :: generated) next
    in
    fun () -> aux [] [Gen]
end

module AdHocGadt = struct
  let free_size =
    let rec aux size todo =
      if todo = 0 then size
      else if Rand.bool () then aux size (todo - 1)
      else aux (size + 1) (todo + 1)
    in
    fun () -> aux 0 1

  type zero = |
  type _ succ = |

  type (_, _) stack =
    |  []  : ('a, zero) stack
    | (::) : 'a * ('a, 'n) stack -> ('a, 'n succ) stack

  type _ tasks =
    | Nil: zero succ tasks
    | Build: 'n succ tasks -> 'n succ succ tasks
    | Gen: 'n succ tasks -> 'n tasks

  let free_gen =
    let rec aux: type n. (BinTree.t, n) stack -> n tasks -> BinTree.t
      = fun generated tasks -> match tasks with
        | Nil -> let [x] = generated in x
        | Gen next ->
          if Rand.bool () (* XXX. *)
          then aux (BinTree.Leaf :: generated) next
          else aux generated (Gen (Gen (Build next)))
        | Build next ->
          let x :: y :: generated = generated in
          aux (BinTree.Node (x, y) :: generated) next
    in
    fun () -> aux [] (Gen Nil)
end

let ad_hoc =
  AdHoc.free_size, (fun () -> ignore (AdHoc.free_gen ()))

let ad_hoc_gadt =
  AdHocGadt.free_size, (fun () -> ignore (AdHoc.free_gen ()))

let arbogen =
  let open Implementations.Arbogen in
  let grammar = Expr.(
    Union (Epsilon, Product (Z, Product (Ref 0, Ref 0)))
  ) in
  (fun () -> free_size grammar Rand.bool),
  (fun () -> ignore (free_gen grammar Rand.bool))

let arbogen_alt =
  let open Implementations.ArbogenAlt in
  let grammar = Expr.(
    Union (Epsilon, Product (Z, Product (Ref 0, Ref 0)))
  ) in
  (fun () -> free_size grammar Rand.bool),
  (fun () -> ignore (free_gen grammar Rand.bool))

let arbogen_alt_2 =
  let open Implementations.ArbogenAlt2 in
  let grammar = Expr.(
    Union (Epsilon, Product (Z, Product (Ref 0, Ref 0)))
  ) in
  (fun () -> free_size grammar Rand.bool),
  (fun () -> ignore (free_gen grammar Rand.bool))

let gadt =
  let open Implementations.Gadt in
  let grammar = Expr.(
    Union (Epsilon, Product (Z, Product (Ref 0, Ref 0)))
  ) in
  let builder =
    union_builder
      (fun () -> BinTree.Leaf)
      (fun (_, (l, r)) -> BinTree.Node (l, r))
  in
  (fun () -> free_size grammar Rand.bool),
  (fun () -> ignore (free_gen grammar builder Rand.bool))

let () =
  Rand.init 41329213424289;

  let samplers = [
    "ad-hoc", ad_hoc;
    "ad-hoc-gadt", ad_hoc_gadt;
    "arbogen", arbogen;
    "arbogen-alt", arbogen_alt;
    "arbogen-alt-2", arbogen_alt_2;
    "gadt", gadt;
  ] in
  benchall samplers
