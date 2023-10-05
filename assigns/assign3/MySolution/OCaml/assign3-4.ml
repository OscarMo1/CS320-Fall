#use "./../../../../classlib/OCaml/MyOCaml.ml"

let list_of_buddies (word: string): string list =
  let n = String.length word in
  let buddy_at i c =
    let prefix = String.sub word 0 i in
    let suffix = String.sub word (i + 1) (n - i - 1) in
    prefix ^ (String.make 1 c) ^ suffix
  in
  let buddies =
    List.flatten (List.init n (fun i ->
      let c = word.[i] in
      let replacements = List.filter (fun x -> x <> c) ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'] in
      List.map (buddy_at i) replacements
    ))
  in
  buddies
;;


