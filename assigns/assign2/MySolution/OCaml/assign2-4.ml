#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml"
let string_sepjoin_list (sep: string) (xs: string list): string =
  let concat = List.fold_right (^) xs "" in
  let len = String.length concat in
  let sep_len = String.length sep in
  if len = 0 || sep_len = 0 then concat
  else
    let result = Bytes.create (len + (List.length xs - 1) * sep_len) in
    let rec loop i j =
      if i = len then Bytes.to_string result
      else begin
        Bytes.set result j concat.[i];
        if i < len - 1 then begin
          for k = 0 to sep_len - 1 do
            Bytes.set result (j + k + 1) sep.[k]
          done;
          loop (i + 1) (j + sep_len + 1)
        end
        else loop (i + 1) (j + 1)
      end
    in
    loop 0 0
;;



