
let intrev10(n: int): int =
let rec loop(n: int)(reversed: int): int =
  if n = 0 then
    reversed
  else
    let reversed = reversed * 10 + (n mod 10) in
    let remaining = n / 10 in
    loop remaining reversed
in
loop n 0
