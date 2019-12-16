type void = |
type ('a, 'b) either = Left of 'a | Right of 'b

(** {2 Epressions} *)

module Expr = struct
  (** [(a, b) t] = expressions of shape [b] with holes of type [a] *)
  type (_, _) t =
    | Epsilon: ('a, unit) t
    | Z: ('a, unit) t
    | Ref: int -> ('a, 'a) t
    | Union: ('a, 'b) t * ('a, 'c) t -> ('a, ('b, 'c) either) t
    | Product: ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
end

(** {2 Simulation} *)

type expr_list = 
  | [] : expr_list
  | (::) : _ Expr.t * expr_list -> expr_list

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

module Stack = struct
  type _ t =
    | Nil: void t
    | Cons: 'a * 'b t -> ('a * 'b) t

  let head: type a b. (a * b) t -> a = function
    | Cons (x, _) -> x

  let pop: type a b. (a * b) t -> a * b t = function
    | Cons (x, y) -> x, y

  let push: type a b. a -> b t -> (a * b) t =
    fun x xs -> Cons (x, xs)
end

type (_, _, _) task =
  | Noop: ('a, 's, 's) task
  | Gen: ('a, 'b) Expr.t * ('a, 'b * 's, 't) task -> ('a, 's, 't) task
  | Build: ('b -> 'a) * ('a, 'a * 's, 't) task -> ('a, 'b * 's, 't) task
  | WrapL: ('a, ('b, _) either * 's, 't) task -> ('a, 'b * 's, 't) task
  | WrapR: ('a, (_, 'b) either * 's, 't) task -> ('a, 'b * 's, 't) task
  | Pair: ('a, ('b * 'c) * 's, 't) task -> ('a, 'b * ('c * 's), 't) task

let free_gen (type a b) (expr: (a, b) Expr.t) (builder: b -> a) rand_bool : a =
  let rec loop: type s t. s Stack.t -> (a, s, a * t) task -> a
  = fun generated task -> match task with
    | Noop -> Stack.head generated

    | Gen (Epsilon, next) -> loop (Stack.push () generated) next
    | Gen (Z, next) -> loop (Stack.push () generated) next
    | Gen (Ref _, next) -> loop generated (Gen (expr, Build (builder, next)))
    | Gen (Union (e1, e2), next) ->
      if rand_bool () (* XXX. *) then
        loop generated (Gen (e1, WrapL next))
      else
        loop generated (Gen (e2, WrapR next))
    | Gen (Product (e1, e2), next) ->
      loop generated (Gen (e2, Gen (e1, Pair next)))

    | WrapL next ->
      let x, generated = Stack.pop generated in
      loop (Stack.push (Left x) generated) next
    | WrapR next ->
      let x, generated = Stack.pop generated in
      loop (Stack.push (Right x) generated) next
    | Pair next ->
      let x, generated = Stack.pop generated in
      let y, generated = Stack.pop generated in
      let generated = Stack.push (x, y) generated in
      loop generated next
    | Build (builder, next) ->
      let x, generated = Stack.pop generated in
      loop (Stack.push (builder x) generated) next
  in
  loop Stack.Nil (Gen (Expr.Ref 0, Noop))

let union_builder f g = function
  | Left x -> f x
  | Right y -> g y

let rec size_builder: type b. (int, b) Expr.t -> (b -> int)
  = function
    | Epsilon -> (fun () -> 0)
    | Z -> (fun () -> 1)
    | Ref _ -> (fun n -> n)
    | Product (e1, e2) ->
      let s1 = size_builder e1 in
      let s2 = size_builder e2 in
      (fun (x, y) -> s1 x + s2 y)
    | Union (e1, e2) ->
      union_builder (size_builder e1) (size_builder e2)
