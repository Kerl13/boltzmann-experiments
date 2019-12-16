include Arbogen

type zero = |
type _ succ = |

type (_, _) stack =
  | [] : ('a, zero) stack
  | (::) : 'a * ('a, 'n) stack -> ('a, 'n succ) stack

type _ tasks =
  | Nil: zero tasks
  | Gen: int Expr.t * 'n tasks -> 'n tasks
  | Build: 'n tasks -> 'n succ tasks

let unwrap: 'a list -> 'a = function
  | [x] -> x
  | _ -> invalid_arg "unwrap"

type tree = GenericTree.t

let free_size grammar = free_size grammar Rand.bool

let free_gen grammar =
  let rec loop: type n. tree list -> (tree list, n) stack -> n tasks -> tree
    = fun current generated tasks -> match tasks with
      | Nil -> unwrap current

      | Gen (Epsilon, next) -> loop current generated next
      | Gen (Z, next) -> loop current generated next
      | Gen (Union (e1, e2), next) ->
        if Rand.bool () (* XXX. *) then
          loop current generated (Gen (e1, next))
        else
          loop current generated (Gen (e2, next))
      | Gen (Product (e1, e2), next) ->
        loop current generated (Gen (e2, Gen (e1, next)))
      | Gen (Ref _, next) ->
        loop [] (current :: generated) (Gen (grammar, Build next))

      | Build next ->
        let tree = GenericTree.Node current in
        let current :: generated = generated in
        loop (tree :: current) generated next
  in
  loop [] [] (Gen (Ref 0, Nil))
