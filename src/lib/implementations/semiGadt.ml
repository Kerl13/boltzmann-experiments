type zero = |
type _ succ = |

(** {2 A more adapted generic tree module *)

module GenericTree = struct
  type t =
    | Epsilon
    | Atom
    | Node of t list
end

type tree = GenericTree.t

(** {2 Expressions} *)

module Expr = struct
  type t =
    | Epsilon: t
    | Z: t
    | Ref: int -> t
    | Union: t * t -> t
    | Product: t * t -> t
end

(** {2 Simulation} *)

type expr_list =
  | [] : expr_list
  | (::) : Expr.t * expr_list -> expr_list

let free_size grammar rand_bool =
  let rec loop size = function
    | [] -> size

    | Epsilon :: next -> loop size next
    | Z :: next -> loop (size + 1) next
    | Ref _ :: next -> loop size (grammar :: next)
    | Union (e1, e2) :: next ->
      if rand_bool () (* XXX. *)
      then loop size (e1 :: next)
      else loop size (e2 :: next)
    | Product (e1, e2) :: next -> loop size (e2 :: e1 :: next)
  in
  loop 0 [Ref 0]

(** {2 Generation} *)

type (_, _) stack =
  | [] : ('a, zero) stack
  | (::) : 'a * ('a, 'l) stack -> ('a, 'l succ) stack

type _ tasks =
  | Finish: (zero succ) tasks
  | Gen: Expr.t * ('l succ) tasks -> 'l tasks
  | Build: ('l succ) tasks -> 'l succ succ tasks

(* let free_gen grammar rand_bool = *)
  (* let rec loop: type l. (tree, l) stack -> l tasks -> tree *)
    (* = fun generated tasks -> *)
      (* match tasks with *)
      (* | Finish -> let [x] = generated in x *)
(*  *)
      (* | Gen (Epsilon, next) -> loop (Epsilon :: generated) next *)
      (* | Gen (Z, next) -> loop (Atom :: generated) next *)
      (* | Gen (Union (e1, e2), next) -> *)
        (* if rand_bool () (* XXX. *) *)
        (* then loop generated (Gen e1 :: next) *)
        (* else loop generated (Gen e2 :: next) *)
      (* | Gen (Product (e1, e2), next) -> *)
        (* loop  *)
  (* in *)
  (* gen [] (Gen Finish) *)
