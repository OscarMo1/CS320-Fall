(* ****** ****** *)
#use "./../MySolution/assign1-2.ml";;
(* ****** ****** *)

let string_merge(cs1: string)(cs2: string): string =
  let merged = string_make_fwork (fun work ->
    let rec merge_strings i1 i2 =
      if i1 < String.length cs1 && i2 < String.length cs2 then (
        let c1 = String.get cs1 i1 in
        let c2 = String.get cs2 i2 in
        if c1 <= c2 then (
          work c1;
          merge_strings (i1 + 1) i2
        ) else (
          work c2;
          merge_strings i1 (i2 + 1)
        )
      ) else if i1 < String.length cs1 then (
        work (String.get cs1 i1);
        merge_strings (i1 + 1) i2
      ) else if i2 < String.length cs2 then (
        work (String.get cs2 i2);
        merge_strings i1 (i2 + 1)
      ) else ()
    in
    merge_strings 0 0
  ) in
  merged
;;


