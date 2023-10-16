#use "./../../../../classlib/OCaml/MyOCaml.ml"



  let rec series sum sign x =
    fun () ->
      let next = 1.0 /. x in
      (* finds next sum using appropriate sign *)
      let nSum = 
        if sign then 
          sum +. next 
        else 
          sum -. next 
      in
      (* constructs the stream by calling series *)
      StrCons(nSum, series nSum (not sign) (x +. 1.0))
  ;;
  
  let the_ln2_stream: float stream = series 0.0 true 1.0;;