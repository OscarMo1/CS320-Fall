#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml"

let string_sepjoin_list (sep: string) (xs: string list): string =
  let combine = list_foldleft xs "" (fun s1 s2 -> if s2 = "" then s1 else string_appe) in
  combine
