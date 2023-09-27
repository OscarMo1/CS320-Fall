#use "./../../assign2.ml";;
let string_sepjoin_list (sep: string) (xs: string list): string =
  let combine = list_foldright xs "" (fun s1 s2 -> if s2 = "" then s1 else s1 ^ sep ^ s2) in
  combine
