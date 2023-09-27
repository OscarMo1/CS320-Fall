#use "./../../assign2.ml";;
type ('xs, 'x0) iforeach = 'xs -> ('x0 -> unit) -> unit

  let foldleft_to_iforeach
    (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
    fun xs work ->
      let _ = foldleft xs 0 (fun _ x -> work x) in ()
  
