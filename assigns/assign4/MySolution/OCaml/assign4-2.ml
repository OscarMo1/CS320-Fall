#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec nat_pairs_stream i j () =
  StrCons((i, j), fun () -> nat_pairs_stream i (j + 1) ())
;;

let theNatPairs: (int * int) stream = nat_pairs_stream 0 0;;
