#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec series curVal sign n =
  fun () ->
    let nextVal = 1.0 /. n in
    let nSum = 
      if sign then 
        curVal +. nextVal
      else 
        curVal -. nextVal
    in
    StrCons(nSum, series nSum (not sign) (n +. 1.0))
;;

let the_ln2_stream: float stream = series 0.0 true 1.0;;


