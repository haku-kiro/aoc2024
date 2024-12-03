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

let rec match_procedure flag current = function
  | [] -> current (* Empty list, return current *)
  | [ x ] ->
    (* Single element left, decide based on flag *)
    if flag then current @ [ x ] else current
  | "do()" :: rest -> match_procedure true current rest
  | "don't()" :: rest -> match_procedure false current rest
  | x :: rest ->
    (* General case: process head and tail *)
    if flag
    then match_procedure true (current @ [ x ]) rest
    else match_procedure false current rest
;;

let part02 filename =
  let data = String.concat "" (Helpers.File.read_file filename) in
  (* Important note, we're matching on a group, i.e '(re)' so that we can
     flag the "do()" and "don't()" *)
  let split_pattern = Pcre.regexp "(do\\(\\)|don't\\(\\))" in
  let splits = Pcre.split ~rex:split_pattern data in
  let working_data = match_procedure true [] splits in
  (* now similar to the first part *)
  let r = "mul\\((\\d{1,3},\\d{1,3})\\)" in
  let result =
    working_data
    |> List.map (all_matches r 1)
    |> List.map get_number_pairs
    |> List.map (fun a ->
      let left, right = a in
      List.fold_left2 (fun count x y -> count + (x * y)) 0 left right)
    |> List.fold_left (fun sum x -> sum + x) 0
  in
  result
;;
