(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml"
(* ****** ****** *)
let rec intrep_add(ds1: string)(ds2: string): string =
  let len1 = string_length ds1 in
  let len2 = string_length ds2 in

  (* Helper function to perform addition *)
  let rec add_strings i j carry result =
    if i < 0 && j < 0 then
      if carry = 1 then "1" ^ result else result
    else
      let digit1 = if i >= 0 then digit_of_char (string_get_at ds1 i) else 0 in
      let digit2 = if j >= 0 then digit_of_char (string_get_at ds2 j) else 0 in
      let sum = digit1 + digit2 + carry in
      let new_digit = char_of_digit (sum mod 10) in
      let new_carry = sum / 10 in
      let new_result = string_cons new_digit result in
      add_strings (i - 1) (j - 1) new_carry new_result
  in

  let result =
    if len1 > len2 then
      add_strings (len1 - 1) (len2 - 1) 0 ""
    else
      add_strings (len2 - 1) (len1 - 1) 0 ""
  in

  result
;;
