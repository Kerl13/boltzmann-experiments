include Arbogen

let pop = function
  | x :: xs -> x, xs
  | _ -> invalid_arg "pop"

let unwrap = function
  | [x] -> x
  | _ -> invalid_arg "unwrap"

type instruction =
  | Gen of int Expr.t
  | Build of int


let build generated depth =
  let rec loop generated children depth =
    if depth = 0 then children, generated
    else match generated with
      | tree :: generated -> loop generated (tree :: children) (depth - 1)
      | [] -> invalid_arg "build"
  in
  loop generated [] depth

let free_gen grammar rand_bool =
  let rec loop depth generated = function
    | [] -> unwrap generated

    | Gen Epsilon :: next -> loop depth generated next
    | Gen Z :: next -> loop depth generated next
    | Gen (Union (e1, e2)) :: next ->
      if rand_bool () (* XXX. *) then
        loop depth generated (Gen e1 :: next)
      else
        loop depth generated (Gen e2 :: next)
    | Gen (Product (e1, e2)) :: next ->
      loop depth generated (Gen e2 :: Gen e1 :: next)
    | Gen (Ref _) :: next ->
      loop 0 generated (Gen grammar :: Build depth :: next)

    | Build i :: next ->
      let children, generated = build generated depth in
      let tree = GenericTree.Node children in
      loop (i + 1) (tree :: generated) next
  in
  loop 0 [] [Gen (Ref 0)]
