#use "./../../assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml"


let rec matrix_transpose(xss: 'a list list): 'a list list =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ ->
    let column = list_make_fwork(
      fun work -> list_foreach xss (fun row -> match row with
        | [] -> ()
        | x :: _ -> work(x)
      )
    ) in
    let rest = list_make_fwork(
      fun work -> list_foreach xss (fun row -> match row with
        | [] -> work([])
        | _ :: xs -> work(xs)
      )
    ) in
    column :: matrix_transpose rest
