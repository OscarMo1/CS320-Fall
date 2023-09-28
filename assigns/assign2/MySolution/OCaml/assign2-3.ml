#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml"

let foldleft_to_iforeach (foldleft: ('xs, 'x0, 'r0) foldleft): ('xs, 'x0) iforeach =
  fun xs f ->
    let rec loop index = function
      | [] -> ()
      | x :: xs' ->
        f index x;
        loop (index + 1) xs'
    in
    loop 0 xs
    ;;
