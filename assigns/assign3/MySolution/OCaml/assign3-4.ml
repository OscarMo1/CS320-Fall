#use "./../../../../classlib/OCaml/MyOCaml.ml"

let string_sub (s: string) (start: int) (len: int): string =
  let l = string_length s in
  if start < 0 && start >= l && len <= 0 then
    ""
  else if start + len > l then
    string_tabulate len (fun i -> string_get_at s (start + i))
  else
    string_tabulate len (fun i -> string_get_at s (start + i))
;;

let rec my_map(xs)(fopr: 'a -> 'b): 'b list =
  match xs with
  | [] -> []
  | x1 :: xs -> fopr(x1) :: my_map(xs)(fopr)
;;

let list_filter (xs: 'a list) (test: 'a -> bool): 'a list =
  list_foldright xs [] 
  (fun x acc ->
    if test x then 
      x :: acc 
    else 
      acc)
;;

let flatten (lst: 'a list list): 'a list =
  list_foldleft lst [] list_append
;;

let list_of_buddies (word: string): string list =
  let len = string_length word in
  let swap i c =
    let prefix = string_sub word 0 i in
    let suffix = string_sub word (i + 1) (len - i - 1) in
    string_concat_list [prefix; str(c); suffix]
  in
  let chars = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in
  let buddies =
    flatten (
      my_map (int1_listize len) (fun i ->
        let char = string_get_at word i in
        let swapped = list_filter chars (fun x -> x <> char) in
        my_map swapped (fun swapped -> swap i swapped) 
      )
    )
  in
  buddies
;;