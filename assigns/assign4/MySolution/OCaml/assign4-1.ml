#use "./../../../../classlib/OCaml/MyOCaml.ml"


(* Function to create a stream of partial sums of the ln 2 series *)
let the_ln2_stream: float stream =
  let rec helper (n: int) (sign: float) (sum: float): float stream =
    fun () ->
      StrCons(sum, fun () -> helper (n + 1) (-.sign) (sum +. sign /. float_of_int (n + 1)) ())
  in
  helper 1 1. 1. (* start from the first term *)


