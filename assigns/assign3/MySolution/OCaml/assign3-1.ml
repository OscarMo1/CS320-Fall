

#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec matrix_transpose (xss: 'a list list): 'a list list =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ ->
    let rec get_first_column xss =
      match xss with
      | [] -> []
      | row :: rest -> List.hd row :: get_first_column rest
    in
    let rec get_rest_columns xss =
      match xss with
      | [] -> []
      | row :: rest -> List.tl row :: get_rest_columns rest
    in
    List.append [get_first_column xss] (matrix_transpose (get_rest_columns xss))
;;