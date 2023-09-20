#use "./../../../classlib/OCaml/MyOCaml.ml"

let intrep_add(ds1: string)(ds2: string): string =
  (* initialize string length*)
  let ds1_length = string_length ds1 in
  let ds2_length = string_length ds2 in
  (* recursive function that iterates through string and adds and accounts sum and carry*)
  let rec create_num i j num carry =
    if i < 0 && j < 0 then
      if carry = 1 then string_cons '1' num else num
    else
      (*gets values by converting char to int*)
      let digit1 = if i >= 0 then ord (string_get_at ds1 i) - 48 else 0 in
      let digit2 = if j >= 0 then ord (string_get_at ds2 j) - 48 else 0 in
      (* if sum of digit1 and digit2 > 9, then mod 10 and there is a carry*)
      let digit_sum = digit1 + digit2 + carry in
      let new_carry = if digit_sum > 9 then 1 else 0 in
      let new_num = string_cons (chr (digit_sum mod 10 + 48)) num in

      create_num (i - 1) (j - 1) new_num new_carry
  in

  create_num (ds1_length - 1) (ds2_length - 1) "" 0
;;
