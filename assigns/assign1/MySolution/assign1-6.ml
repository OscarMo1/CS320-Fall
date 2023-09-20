#use "./../../../classlib/OCaml/MyOCaml.ml"
let is_1324_like(a: char)(b: char)(c: char)(d: char): bool =
  a < c && c < b && b < d
;;

let rec has_1324_like_subsequence(cs: string): bool =
  let len = string_length cs in
  if len < 4 then
    false
  else
    let a = string_get_at cs 0 in
    let b = string_get_at cs 1 in
    let c = string_get_at cs 2 in
    let d = string_get_at cs 3 in
    if is_1324_like a b c d then
      true
    else
      has_1324_like_subsequence (string_tail cs)
;;

let string_avoid_1324(cs: string): bool =
  not (has_1324_like_subsequence cs)
;;



