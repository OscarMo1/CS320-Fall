#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml"

let foldleft_to_iforeach (foldleft: ('xs, 'x0, 'r0) foldleft): ('xs, 'x0) iforeach =
  fun xs f ->
    let rec iterate index = function
      | [] -> ()
      | x :: xs' ->
        f index x;
        iterate (index + 1) xs'
    in
    iterate 0 xs
