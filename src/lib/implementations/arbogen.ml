module GenericTree = struct
  type t = Node of t list
end

(** {2 Expressions} *)

module Expr = struct
  type 'ref t =
    | Epsilon
    | Z
    | Ref of 'ref
    | Product of 'ref t * 'ref t
    | Union of 'ref t * 'ref t
end

(** {2 Simulation} *)

let free_size grammar rand_bool =
  let open Expr in
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

type instruction =
  | Gen of int Expr.t
  | Build

let unwrap = function
  | [Some x] -> x
  | _ -> invalid_arg "unwrap"

let pop2 = function
  | x :: y :: xs -> x, y, xs
  | _ -> invalid_arg "pop2"

let rec build generated children = match generated with
  | None :: generated -> generated, children
  | Some tree :: generated -> build generated (tree :: children)
  | [] -> invalid_arg "build"

let free_gen grammar rand_bool =
  let rec loop generated = function
    | [] -> unwrap generated

    | Gen Epsilon :: next -> loop generated next
    | Gen Z :: next -> loop generated next
    | Gen (Union (e1, e2)) :: next ->
      if rand_bool () (* XXX. *) then
        loop generated (Gen e1 :: next)
      else
        loop generated (Gen e2 :: next)
    | Gen (Product (e1, e2)) :: next ->
      loop generated (Gen e2 :: Gen e1 :: next)
    | Gen (Ref _) :: next ->
      loop (None :: generated) (Gen grammar :: Build :: next)

    | Build :: next ->
      let generated, children = build generated [] in
      let tree = GenericTree.Node children in
      loop (Some tree :: generated) next
  in
  loop [] [Gen (Ref 0)]
