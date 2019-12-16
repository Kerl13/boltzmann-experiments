type void = |
type ('a, 'b) either = Left of 'a | Right of 'b

module Tree = struct
  type t =
    | Leaf
    | Node of t * t

  let rec pp fmt = function
    | Leaf -> Format.pp_print_string fmt "⋅"
    | Node (l, r) -> Format.fprintf fmt "[%a,%a]" pp l pp r
end

module Gadt = struct
  module Expr = struct
    type (_, _) t =
      | Z: ('a, unit) t
      | Ref: int -> ('a, 'a) t
      | Union: ('a, 'b) t * ('a, 'c) t -> ('a, ('b, 'c) either) t
      | Product: ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
  end

  let gen (type a b) (expr: (a, b) Expr.t) (builder: b -> a) : a =
    let rec loop: type b. (a, b) Expr.t -> b = function
      | Expr.Z -> ()
      | Expr.Ref _ -> builder (loop expr)
      | Expr.Product (e1, e2) -> (loop e1, loop e2)
      | Expr.Union (e1, e2) ->
        if Random.bool () (* XXX. *) then
          Left (loop e1)
        else
          Right (loop e2)
    in
    loop (Ref 0)

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

  let gen_tr (type a b) (expr: (a, b) Expr.t) (builder: b -> a) : a =
    let rec loop: type s t. s Stack.t -> (a, s, a * t) task -> a
    = fun generated task -> match task with
      | Noop -> Stack.head generated
      | Gen (Z, next) -> loop (Stack.push () generated) next
      | Gen (Ref _, next) -> loop generated (Gen (expr, Build (builder, next)))
      | Gen (Union (e1, e2), next) ->
        if Random.bool () (* XXX. *) then
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
      | Z -> (fun () -> 1)
      | Ref _ -> (fun n -> n)
      | Product (e1, e2) ->
        let s1 = size_builder e1 in
        let s2 = size_builder e2 in
        (fun (x, y) -> s1 x + s2 y)
      | Union (e1, e2) ->
        union_builder (size_builder e1) (size_builder e2)

  module Catalan = struct
    let grammar = Expr.(Union (Z, Product (Ref 0, Ref 0)))

    let build_tree =
      union_builder
        (fun () -> Tree.Leaf)
        (fun (l, r) -> Tree.Node (l, r))

    let build_dyck =
      union_builder
        (fun () -> "")
        (fun (l, r) -> "(" ^ l ^ ")" ^ r)
  end
end

module Naive = struct
  type side = Left | Right

  type 'a build_step =
    | Container of 'a
    | Atom
    | Prod of 'a build_step * 'a build_step
    | Choice of side * 'a build_step

  let pp_side fmt = function
    | Left -> Format.pp_print_string fmt "Left"
    | Right -> Format.pp_print_string fmt "Right"

  let rec pp_bs ~pp fmt = function
    | Container x -> Format.fprintf fmt "Container(%a)" pp x
    | Atom -> Format.pp_print_string fmt "Atom"
    | Prod (x, y) -> Format.fprintf fmt "Prod(%a, %a)" (pp_bs ~pp) x (pp_bs ~pp) y
    | Choice (side, x) -> Format.fprintf fmt "Choice(%a, %a)" pp_side side (pp_bs ~pp) x

  type expr =
    | Z
    | Ref of int
    | Union of expr * expr
    | Product of expr * expr

  type instr =
    | Gen of expr
    | Pair
    | Call of int
    | WrapChoice of side

  let pop2 = function
    | x :: y :: trees -> x, y, trees
    | _ -> invalid_arg "pop2"

  let pop = function
    | x :: xs -> x, xs
    | _ -> invalid_arg "pop"

  let gen defs builders =
    let rec gen trees = function
      | [] -> trees

      | Gen Z :: next ->
        gen (Atom :: trees) next

      | Gen (Ref i) :: next ->
        gen trees (Gen defs.(i) :: Call i :: next)

      | Gen (Union (x, y)) :: next ->
        if Random.bool () (* XXX. *)
        then gen trees (Gen x :: WrapChoice Left :: next)
        else gen trees (Gen y :: WrapChoice Right :: next)

      | Gen (Product (x, y)) :: next ->
        gen trees (Gen y :: Gen x :: Pair :: next)

      | Pair :: next ->
        let x, y, trees = pop2 trees in
        gen (Prod (x, y) :: trees) next

      | Call i :: next ->
        let tree, trees = pop trees in
        let tree = Container (builders.(i) tree) in
        gen (tree :: trees) next

      | WrapChoice side :: next ->
        let tree, trees = pop trees in
        gen (Choice (side, tree) :: trees) next
    in
    match gen [] [Gen (Ref 0)] with
    | [Container tree] -> tree
    | _ -> failwith "unreachable"

  module Builder = struct
    let union f g = function
      | Choice (Left, x) -> f x
      | Choice (Right, y) -> g y
      | _ -> failwith "Builder.union"

    let prod f = function
      | Prod (x, y) -> f x y
      | _ -> invalid_arg "Builder.prod"

    let imm = function
      | Container x -> x
      | _ -> invalid_arg "Builder.imm"

    let const x _ = x
  end

  module Catalan = struct
    let grammar = [|Union (Z, Product (Ref 0, Ref 0))|]

    let build_tree = let open Builder in
      union
        (fun _ -> Tree.Leaf)
        (prod (fun x y -> Tree.Node (imm x, imm y)))

    let build_dyck = let open Builder in
      union
        (fun _ -> "")
        (prod (fun l r -> "(" ^ imm l ^ ")" ^ imm r))
  end
end

let main () =
  let seed = int_of_string Sys.argv.(1) in

  let () =
    Random.init seed;
    let open Naive in
    let t0 = Unix.gettimeofday () in
    let tree = Catalan.(gen grammar [|build_tree|]) in
    let dt = Unix.gettimeofday () -. t0 in
    Format.printf "[Generated in %F seconds]@." dt;
    Format.printf "==> %a@." Tree.pp tree;

    Random.init seed;
    let t0 = Unix.gettimeofday () in
    let tree = Catalan.(gen grammar [|build_dyck|]) in
    let dt = Unix.gettimeofday () -. t0 in
    Format.printf "[Generated in %F seconds]@." dt;
    Format.printf "==> %s@." tree
  in

  let () =
    Random.init seed;
    let open Gadt in
    let t0 = Unix.gettimeofday () in
    let tree = Catalan.(gen_tr grammar build_tree) in
    let dt = Unix.gettimeofday () -. t0 in
    Format.printf "[Generated in %F seconds]@." dt;
    Format.printf "==> %a@." Tree.pp tree;

    Random.init seed;
    let t0 = Unix.gettimeofday () in
    let tree = Catalan.(gen_tr grammar build_dyck) in
    let dt = Unix.gettimeofday () -. t0 in
    Format.printf "[Generated in %F seconds]@." dt;
    Format.printf "==> %s@." tree
  in

  Format.printf "Done@."

let () = main ()
