#use "./../../../../classlib/OCaml/MyOCaml.ml"

let theNatPairs: (int * int) stream =
  (* main call function *)
  let rec pair x i j =
    fun () ->
      StrCons((i, j), next_pair x i j)
  (* co-helper function *)
  and next_pair x i j =
  (* iterates all combinations that add to x, then moves to x+1 *)
  if i = 0 then
    pair (x+1) (x+1) 0
  else
    pair x (i-1) (j+1)
  in
  pair 0 0 0
;;