(** {2 A generic generator} *)

module StackBased = struct
  type task = Gen | Build

  let unwrap = function
    | [x] -> x
    | _ -> invalid_arg "unwrap"

  let pop2 = function
    | x :: y :: xs -> x, y, xs
    | _ -> invalid_arg "pop2"

  let free_gen leaf node rand_bool =
    let rec aux generated = function
      | [] -> unwrap generated
      | Gen :: tasks ->
        if rand_bool () then
          aux (leaf :: generated) tasks
        else
          aux generated (Gen :: Gen :: Build :: tasks)
      | Build :: tasks ->
        let x, y, generated = pop2 generated in
        aux (node x y :: generated) tasks
    in
    aux [] [Gen]

  let free_size =
    let leaf = 0 in
    let node x y = x + y + 1 in
    free_gen leaf node
end

(** Generic generator (can handle tree and size generation). *)
module Generic = struct
  type task = Gen | Build

  let free_gen ~initial ~finish ~on_leaf ~on_node rand_bool =
    let rec aux state = function
      | [] -> finish state
      | Gen :: tasks ->
        if rand_bool () then
          aux (on_leaf state) tasks
        else
          aux state (Gen :: Gen :: Build :: tasks)
      | Build :: tasks ->
        aux (on_node state) tasks
    in
    aux initial [Gen]

  let free_size =
    let on_leaf state = state in
    let on_node state = state + 1 in
    let initial = 0 in
    let finish state = state in
    free_gen ~initial ~finish ~on_leaf ~on_node
end

module Gadt = struct
  type zero = |
  type _ succ = S

  type (_, _) stack =
    | []: ('a, zero) stack
    | (::): 'a * ('a, 'l) stack -> ('a, 'l succ) stack

  type _ tasks =
    | Finish: (zero succ) tasks
    | Node: ('l succ) tasks -> 'l succ succ tasks
    | Gen: ('l succ) tasks -> 'l tasks

  let free_gen leaf node rand_bool =
    let rec gen: type l. ('a, l) stack -> l tasks -> 'a
      = fun generated tasks ->
        match tasks with
        | Finish -> let [x] = generated in x

        | Gen tasks ->
          if rand_bool () then
            gen (leaf :: generated) tasks
          else
            gen generated (Gen (Gen (Node tasks)))

        | Node tasks ->
          let x :: y :: generated = generated in
          gen (node x y :: generated) tasks
    in
    gen [] (Gen Finish)

  let free_size =
    let leaf = 0 in
    let node x y = 1 + x + y in
    free_gen leaf node
end

(** {2 Size generators} *)

(** Optimal usable generator(?). Runs in constant space. *)
module ConstantSpace = struct
  let free_size rand_bool =
    let rec aux size todo =
      if todo = 0 then size
      else if rand_bool () then aux size (todo - 1)
      else aux (size + 1) (todo + 1)
    in
    aux 0 1
end

(** Mimic Arbogen's behaviour. *)
module Arbogen = struct
  let free_size rand_bool =
    let rec aux size = function
      | [] -> size
      | _ :: tasks ->
        if rand_bool () then
          aux size tasks
        else
          aux (size + 1) (0 :: 0 :: tasks)
    in
    aux 0 [0]
end
