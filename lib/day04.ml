let data = Helpers.File.read_file "./data/day04.txt"

let make_grid d =
  (* going to assume the columns are fixed at an exact size for each row *)
  let col_a = List.hd d in
  let column_count = String.to_seq col_a |> List.of_seq |> List.length in
  let row_count = List.length d in
  let grid = Array.init row_count (fun _ -> Array.make column_count ' ') in
  d
  |> List.iteri (fun row row_data ->
    let column_data = String.to_seq row_data |> List.of_seq in
    column_data |> List.iteri (fun col char -> grid.(row).(col) <- char));
  grid
;;

let out_of_bounds u r d l x y =
  if x < u
  then true
  else if x > d
  then true
  else if y < l
  then true
  else if y > r
  then true
  else false
;;

(* This assumes bound checks have been done *)
let get_substring_up grid start_i start_j end_i =
  let result = ref [] in
  for i = start_i downto end_i do
    result := !result @ [ grid.(i).(start_j) ]
  done;
  String.of_seq (List.to_seq !result)
;;

let get_substring_up_right grid start_i start_j end_i =
  let result = ref [] in
  let j = ref start_j in
  for i = start_i downto end_i do
    result := !result @ [ grid.(i).(!j) ];
    j := !j + 1
  done;
  let str = String.of_seq (List.to_seq !result) in
  Printf.printf "up right: %s\n" str;
  str
;;

let get_substring_right grid start_i start_j end_j =
  let result = ref [] in
  for j = start_j to end_j do
    result := !result @ [ grid.(start_i).(j) ]
  done;
  String.of_seq (List.to_seq !result)
;;

let get_substring_down_right grid start_i start_j end_i =
  let result = ref [] in
  let j = ref start_j in
  for i = start_i to end_i do
    result := !result @ [ grid.(i).(!j) ];
    j := !j + 1
  done;
  String.of_seq (List.to_seq !result)
;;

let get_substring_down grid start_i start_j end_i =
  let result = ref [] in
  for i = start_i to end_i do
    result := !result @ [ grid.(i).(start_j) ]
  done;
  String.of_seq (List.to_seq !result)
;;

let get_substring_down_left grid start_i start_j end_i =
  let result = ref [] in
  let j = ref start_j in
  for i = start_i to end_i do
    result := !result @ [ grid.(i).(!j) ];
    j := !j - 1
  done;
  String.of_seq (List.to_seq !result)
;;

let get_substring_left grid start_i start_j end_j =
  let result = ref [] in
  for j = start_j downto end_j do
    result := !result @ [ grid.(start_i).(j) ]
  done;
  String.of_seq (List.to_seq !result)
;;

let get_substring_up_left grid start_i start_j end_i =
  let result = ref [] in
  let j = ref start_j in
  for i = start_i downto end_i do
    result := !result @ [ grid.(i).(!j) ];
    j := !j - 1
  done;
  String.of_seq (List.to_seq !result)
;;

let part01_match_func grid i j =
  let result = ref [] in
  (* given a match, generate substrings in each direction where possible *)
  let right_bound = Array.length grid.(0) - 1 in
  let lower_bound = Array.length grid - 1 in
  let word_length = 4 in
  let ml = word_length - 1 in
  (* Assuming "XMAS" has length 4 *)
  let up_final = [| i - ml; j |] in
  let right_final = [| i; j + ml |] in
  let up_right_final = [| up_final.(0); right_final.(1) |] in
  let down_final = [| i + ml; j |] in
  let down_right_final = [| down_final.(0); right_final.(1) |] in
  let left_final = [| i; j - word_length + 1 |] in
  let down_left_final = [| down_final.(0); left_final.(1) |] in
  let up_left_final = [| up_final.(0); left_final.(1) |] in
  (* Creating a function from the out_of_bounds function, with pre-passed
     parameters *)
  let out_check = out_of_bounds 0 right_bound lower_bound 0 in
  (* Checks for out of bounds, otherwise get the substring *)
  (* up *)
  if not (out_check up_final.(0) up_final.(1))
  then result := !result @ [ get_substring_up grid i j up_final.(0) ];
  (* up right *)
  if not (out_check up_right_final.(0) up_right_final.(1))
  then
    result
    := !result @ [ get_substring_up_right grid i j up_right_final.(0) ];
  (* right *)
  if not (out_check right_final.(0) right_final.(1))
  then result := !result @ [ get_substring_right grid i j right_final.(1) ];
  (* down right *)
  if not (out_check down_right_final.(0) down_right_final.(1))
  then
    result
    := !result @ [ get_substring_down_right grid i j down_right_final.(0) ];
  (* down *)
  if not (out_check down_final.(0) down_final.(1))
  then result := !result @ [ get_substring_down grid i j down_final.(0) ];
  (* down left *)
  if not (out_check down_left_final.(0) down_left_final.(1))
  then
    result
    := !result @ [ get_substring_down_left grid i j down_left_final.(0) ];
  (* left *)
  if not (out_check left_final.(0) left_final.(1))
  then result := !result @ [ get_substring_left grid i j left_final.(1) ];
  (* up left *)
  if not (out_check up_left_final.(0) up_left_final.(1))
  then
    result := !result @ [ get_substring_up_left grid i j up_left_final.(0) ];
  !result |> List.filter (fun word -> word = "XMAS") |> List.length
;;

let walk_grid grid match_char match_func =
  let acc = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(i) - 1 do
      if grid.(i).(j) = match_char then acc := !acc + match_func grid i j
    done
  done;
  !acc
;;

let part01 =
  let grid = make_grid data in
  let total = walk_grid grid 'X' part01_match_func in
  total
;;
