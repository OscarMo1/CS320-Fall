

#use "./../../../../classlib/OCaml/MyOCaml.ml"

let list_head lst =
  match lst with
  | [] -> 0
  | x :: _ -> x

let list_tail lst =
  match lst with
  | [] -> []
  | _ :: x -> x

let rec matrix_foreach work lst =
  match lst with
  | [] -> []
  | x :: xs -> (work x) :: (matrix_foreach work xs)

let rec matrix_transpose(xss: 'a list list): 'a list list = 
    match xss with
    | [] -> []
    | [] :: _ -> []
    | _ ->
      let col = matrix_foreach list_head xss in
      let tail_matrix = matrix_foreach list_tail xss in
      col :: matrix_transpose tail_matrix