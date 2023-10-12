#use "./../../../../classlib/OCaml/MyOCaml.ml"

type 'a strcon =
  | StrNil
  | StrCons of 'a * (unit -> 'a strcon)

type 'a stream = unit -> 'a strcon

let rec nat_pairs_stream i j () =
  StrCons((i, j), fun () -> nat_pairs_stream i (j + 1) ())
;;

let theNatPairs: (int * int) stream = nat_pairs_stream 0 0;;
