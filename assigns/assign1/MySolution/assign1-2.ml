
#use "./../../../classlib/OCaml/MyOCaml.ml"


let string_merge(cs1: string)(cs2: string): string =
  let merged = string_make_fwork (fun work ->
    let strlen1 = string_length cs1 in
    let strlen2 = string_length cs2 in
    let rec merge_strings i1 i2 =
      if i1 < strlen1 && i2 < strlen2 then (
        let c1 = string_get_at cs1 i1 in
        let c2 = string_get_at cs2 i2 in
        if c1 <= c2 then (
          work c1;
          merge_strings (i1 + 1) i2
        ) else (
          work c2;
          merge_strings i1 (i2 + 1)
        )
      ) else if i1 < strlen1 then (
        let rec merge_remaining_characters i1 i2 work cs1 =
          if i1 < string_length cs1 then (
            work (string_get_at cs1 i1);
            merge_remaining_characters (i1 + 1) i2 work cs1
          )
          else if i2 < string_length cs2 then (
            for i = i2 to string_length cs2 - 1 do
              work (string_get_at cs2 i);
            done
          )
        in
        
        merge_remaining_characters i1 i2 work cs1;
        
      ) else if i2 < strlen2 then (
        let rec merge_remaining_characters i1 i2 work cs1 cs2 =
          if i1 < string_length cs1 && i2 < string_length cs2 then (
            let c1 = string_get_at cs1 i1 in
            let c2 = string_get_at cs2 i2 in
            if c1 <= c2 then (
              work c1;
              merge_remaining_characters (i1 + 1) i2 work cs1 cs2
            ) else (
              work c2;
              merge_remaining_characters i1 (i2 + 1) work cs1 cs2
            )
          )
          else if i1 < string_length cs1 then (
            for i = i1 to string_length cs1 - 1 do
              work (string_get_at cs1 i);
            done
          )
          else if i2 < string_length cs2 then (
            for i = i2 to string_length cs2 - 1 do
              work (string_get_at cs2 i);
            done
          )
        in
        
        merge_remaining_characters i1 i2 work cs1 cs2;
        
      )  
    in
    merge_strings 0 0
  ) in
  merged
;;



