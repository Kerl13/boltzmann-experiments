let size_min = 100_000
let size_max = 200_000
let nb_generations = 10

let rec search free_size =
  let state = Rand.get_state () in
  let size = free_size () in
  if size < size_min || size > size_max then
    search free_size
  else
    (size, state)

let rec search_states free_size n =
  if n = 0 then []
  else
    search free_size :: search_states free_size (n - 1)

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

let ad_hoc () =
  let t = Sys.opaque_identity (AdHoc.free_gen ()) in
  ignore t

let ad_hoc_gadt () =
  let t = Sys.opaque_identity (AdHoc.free_gen ()) in
  ignore t

let arbogen =
  let open Implementations.Arbogen in
  let grammar = Expr.(
    Union (Epsilon, Product (Z, Product (Ref 0, Ref 0)))
  ) in
  let run () =
    let t = Sys.opaque_identity (free_gen grammar Rand.bool) in
    ignore t
  in
  run

let arbogen_alt =
  let open Implementations.ArbogenAlt in
  let grammar = Expr.(
    Union (Epsilon, Product (Z, Product (Ref 0, Ref 0)))
  ) in
  let run () =
    let t = Sys.opaque_identity (free_gen grammar) in
    ignore t
  in
  run

let arbogen_alt_2 =
  let open Implementations.ArbogenAlt2 in
  let grammar = Expr.(
    Union (Epsilon, Product (Z, Product (Ref 0, Ref 0)))
  ) in
  let run () =
    let t = Sys.opaque_identity (free_gen grammar Rand.bool) in
    ignore t
  in
  run

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
  let run () =
    let t = Sys.opaque_identity (free_gen grammar builder) in
    ignore t
  in
  run

let main () =
  Rand.init 41329213424289;

  let sizes, states =
    search_states AdHoc.free_size nb_generations 
    |> List.split
  in
  Format.printf
    "sizes: [%a]@."
    (Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
      Format.pp_print_int)
    sizes;

  let make_test (name, free_gen) =
    let run () =
      List.iter
        (fun state ->
          Rand.set_state state;
          free_gen ())
        states
    in
    (name, run, ())
  in

  let samplers = List.map make_test [
    "ad-hoc", ad_hoc;
    "ad-hoc-gadt", ad_hoc_gadt;
    "arbogen", arbogen;
    "arbogen-alt", arbogen_alt;
    "arbogen-alt-2", arbogen_alt_2;
    "gadt", gadt;
  ] in

  let samples =Benchmark.latencyN ~repeat:7 9L samplers in
  Benchmark.tabulate samples

let () = main ()
