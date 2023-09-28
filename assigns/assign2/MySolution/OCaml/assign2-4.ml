#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml"

let concat (s1: string) (s2: string): string =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let s = String.init (l1 + l2) (fun i -> 
    if i < l1 then
      s1.[i]
    else
      s2.[i - l1])
  in
  s

let rec custom_concat sep xs =
  match xs with
  | [] -> ""
  | [x] -> x
  | x :: rest -> concat x (concat sep (custom_concat sep rest))

let string_sepjoin_list sep xs =
  custom_concat sep xs
