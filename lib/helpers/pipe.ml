(* This function is useful for debugging intermediate values in a pipeline *)
let tap f x =
  let () = f x in
  x
;;
