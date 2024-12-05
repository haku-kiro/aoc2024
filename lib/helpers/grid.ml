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

let make_bound_check grid =
  out_of_bounds 0 (Array.length grid.(0) - 1) (Array.length grid - 1) 0
;;

let get_up grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    for i = start_i downto end_i do
      result := !result @ [ grid.(i).(start_j) ]
    done;
    String.of_seq (List.to_seq !result))
;;

let get_up_right grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    let j = ref start_j in
    for i = start_i downto end_i do
      result := !result @ [ grid.(i).(!j) ];
      j := !j + 1
    done;
    String.of_seq (List.to_seq !result))
;;

let get_right grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    for j = start_j to end_j do
      result := !result @ [ grid.(start_i).(j) ]
    done;
    String.of_seq (List.to_seq !result))
;;

let get_down_right grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    let j = ref start_j in
    for i = start_i to end_i do
      result := !result @ [ grid.(i).(!j) ];
      j := !j + 1
    done;
    String.of_seq (List.to_seq !result))
;;

let get_down grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    for i = start_i to end_i do
      result := !result @ [ grid.(i).(start_j) ]
    done;
    String.of_seq (List.to_seq !result))
;;

let get_down_left grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    let j = ref start_j in
    for i = start_i to end_i do
      result := !result @ [ grid.(i).(!j) ];
      j := !j - 1
    done;
    String.of_seq (List.to_seq !result))
;;

let get_left grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    for j = start_j downto end_j do
      result := !result @ [ grid.(start_i).(j) ]
    done;
    String.of_seq (List.to_seq !result))
;;

let get_up_left grid start_i start_j end_i end_j =
  let bound_check = make_bound_check grid in
  if bound_check end_i end_j
  then ""
  else (
    let result = ref [] in
    let j = ref start_j in
    for i = start_i downto end_i do
      result := !result @ [ grid.(i).(!j) ];
      j := !j - 1
    done;
    String.of_seq (List.to_seq !result))
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
