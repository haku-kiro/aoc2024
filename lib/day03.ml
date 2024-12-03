let all_matches pattern gn text =
  let rex = Pcre.regexp pattern in
  Pcre.exec_all ~rex text
  |> Array.map (fun group -> Pcre.get_substring group gn)
  |> Array.to_list
;;

let get_number_pairs lst =
  let result =
    lst
    |> List.map (String.split_on_char ',')
    |> List.map (function
      | [ x; y ] -> int_of_string x, int_of_string y
      | xs ->
        Printf.printf "[%s]" (String.concat "; " xs);
        failwith "Invalid list length")
    |> List.split
  in
  result
;;

let match_procedure = ()

let part01 filename =
  let r = "mul\\((\\d{1,3},\\d{1,3})\\)" in
  let data = Helpers.File.read_file filename in
  let result =
    data
    |> List.map (all_matches r 1)
    |> List.map get_number_pairs
    |> List.map (fun a ->
      let left, right = a in
      List.fold_left2 (fun count x y -> count + (x * y)) 0 left right)
    |> List.fold_left (fun sum x -> sum + x) 0
  in
  result
;;
