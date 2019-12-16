include Arbogen

let pop = function
  | x :: xs -> x, xs
  | _ -> invalid_arg "pop"

let unwrap = function
  | [x] -> x
  | _ -> invalid_arg "unwrap"

let free_gen grammar rand_bool =
  let rec loop current generated = function
    | [] ->
      assert (generated = []);
      unwrap current

    | Gen Epsilon :: next -> loop current generated next
    | Gen Z :: next -> loop current generated next
    | Gen (Union (e1, e2)) :: next ->
      if rand_bool () (* XXX. *) then
        loop current generated (Gen e1 :: next)
      else
        loop current generated (Gen e2 :: next)
    | Gen (Product (e1, e2)) :: next ->
      loop current generated (Gen e2 :: Gen e1 :: next)
    | Gen (Ref _) :: next ->
      loop [] (current :: generated) (Gen grammar :: Build :: next)

    | Build :: next ->
      let tree = GenericTree.Node current in
      let current, generated = pop generated in
      loop (tree :: current) generated next
  in
  loop [] [] [Gen (Ref 0)]
