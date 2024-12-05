open Helpers.Grid

let part01_match_func grid i j =
  let result = ref [] in
  let word_length = 4 in
  let l = word_length - 1 in
  result := get_up grid i j l :: !result;
  result := get_up_right grid i j l :: !result;
  result := get_right grid i j l :: !result;
  result := get_down_right grid i j l :: !result;
  result := get_down grid i j l :: !result;
  result := get_down_left grid i j l :: !result;
  result := get_left grid i j l :: !result;
  result := get_up_left grid i j l :: !result;
  !result |> List.filter (fun word -> word = "XMAS") |> List.length
;;

let part01 filename =
  let data = Helpers.File.read_file filename in
  let grid = make_grid data in
  let total = walk_grid grid 'X' part01_match_func in
  total
;;

let valid_pairs p1 p2 =
  match p1 with
  | "AM", "AS" | "AS", "AM" ->
    (match p2 with "AM", "AS" -> true | "AS", "AM" -> true | _ -> false)
  | _ -> false
;;

let part02_match_func grid i j =
  let top_right = get_up_right grid i j 1 in
  let bottom_left = get_down_left grid i j 1 in
  let p1 = top_right, bottom_left in
  let top_left = get_up_left grid i j 1 in
  let bottom_right = get_down_right grid i j 1 in
  let p2 = top_left, bottom_right in
  if valid_pairs p1 p2 then 1 else 0
;;

let part02 filename =
  let data = Helpers.File.read_file filename in
  let grid = make_grid data in
  let total = walk_grid grid 'A' part02_match_func in
  total
;;
