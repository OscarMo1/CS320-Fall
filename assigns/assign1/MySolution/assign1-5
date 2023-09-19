(* ****** ****** *)
#use "./../MySolution/assign1-5.ml";;
(* ****** ****** *)

let rec string_longest_ascend(cs: string): string =
  let len = string_length cs in

  (* Helper function to find the longest ascending subsequence *)
  let rec find_longest i current longest =
    if i >= len then
      longest
    else
      let current_char = string_get_at cs i in
      match current with
      | [] -> find_longest (i + 1) [current_char] [current_char]
      | hd :: _ ->
        if current_char <= hd then
          find_longest (i + 1) (current_char :: current) (current_char :: longest)
        else
          find_longest (i + 1) [current_char] (if list_length current > list_length longest then current else longest)
  in

  let longest_sequence = find_longest 0 [] [] in

  (* Convert the list to a string *)
  string_concat_list (list_reverse longest_sequence)
;;


