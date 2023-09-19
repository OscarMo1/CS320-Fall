(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let concat (s1: string) (s2: string): string =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let s = String.init (l1 + l2) (fun i ->
    if i < l1 then string_get(s1, i) else string_get(s2, (i - l1))
  ) in
  s;;

  let int2str(i0: int): string =
    if i0 = 0 then
        "0"
    else
        let rec loop x s = 
            if x = 0 then
                s
            else
                let d = x mod 10 in 
                let c = chr(48 + d) in
                let s1 = str(c) in
                loop (x / 10) (concat s1 s)
        in
        if i0 < 0 then
            concat "-" (loop (-i0) "")
        else
            loop i0 "";;