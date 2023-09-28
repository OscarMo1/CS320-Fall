#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec custom_concat sep xs =
  match xs with
  | [] -> ""
  | [x] -> x
  | x :: rest -> x ^ sep ^ custom_concat sep rest

let string_sepjoin_list sep xs =
  custom_concat sep xs

