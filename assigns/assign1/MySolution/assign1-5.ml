(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml"
(* ****** ****** *)

(* helper function to retrieve the last char in a str*)
let last_char s = 
  let l = string_length(s) in 
  string_get_at s (l-1)
;;

let string_longest_ascend(xs: string): string =
  ( convert xs to a list )
  let l = string_listize xs in
  ( iterate over the list keeping track of a sequence and the longest sequence )
  let rec loop (l: 'a list) (s: string) (ls: string): string =
    match l with
    ( check if s is > ls at the end )
    | [] -> if string_length(s) > string_length(ls) then s else ls
    | h :: t ->
      ( if s != "" check lastchar(s) with the head char index )
      if string_length(s) > 0 && last_char(s) <= h then
        loop t (string_snoc s h) ls 
      else
        ( if lastchar(s) and head aren't in a sequence check if s > ls and set s to h )
          if string_length(s) > string_length(ls) then
            loop t (str(h)) s
          else
            loop t (str(h)) ls
    in
    loop l "" ""
  ;;


