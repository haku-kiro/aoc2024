let rec in_bound bound = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) ->
    let check = abs (x - y) in
    if check > bound || check <= 0 then false else in_bound bound rest
;;

let part01 filename =
  let total =
    Helpers.File.read_file filename
    |> List.map (String.split_on_char ' ')
    |> List.map (List.map int_of_string)
    |> List.map (in_bound 3)
    |> Helpers.Pipe.tap (fun x ->
      print_endline (string_of_int (List.length x));
      let x = List.map string_of_bool x in
      print_endline (String.concat "; " x))
    |> List.fold_left (fun count x -> if x then count + 1 else count) 0
  in
  total
;;
