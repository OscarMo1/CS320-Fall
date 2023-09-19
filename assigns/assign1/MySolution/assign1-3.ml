(* ****** ****** *)
#use "./../MySolution/assign1-3.ml";;
(* ****** ****** *)

let rec string_has_132_like_subsequence(cs: string): bool =
  let len = string_length(cs) in
  if len < 3 then false
  else
    let rec check_subsequence i =
      if i >= len - 2 then false
      else
        let a = string_get_at cs i in
        let c = string_get_at cs (i + 2) in
        let b = string_get_at cs (i + 1) in
        if a < c && c < b then true
        else check_subsequence (i + 1)
    in
    check_subsequence 0 || string_has_132_like_subsequence (string_tail cs)
;;

let string_avoid_132(cs: string): bool =
  not (string_has_132_like_subsequence cs)
;;


