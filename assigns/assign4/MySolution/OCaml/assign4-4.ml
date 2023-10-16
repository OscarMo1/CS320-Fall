#use "./../../../../classlib/OCaml/MyOCaml.ml"

type 'a stream = Stream of 'a * (unit -> 'a stream)

let rec list_permute (xs: 'a list): 'a list stream =
  let rec insert_everywhere x xs =
    match xs with
    | [] -> [[x]]
    | y :: ys -> (x :: xs) :: List.map (fun l -> y :: l) (insert_everywhere x ys)
  in

  let rec insert_into_all_positions x lists =
    match lists with
    | [] -> []
    | hd :: tl -> insert_everywhere x hd @ insert_into_all_positions x tl
  in

  match xs with
  | [] -> Stream ([], fun () -> raise Stream.Failure)
  | x :: xs' ->
    let rec generate_permutations () =
      Stream (x, fun () -> list_permute xs' |> insert_into_all_positions x |> generate_permutations)
    in
    generate_permutations ()
