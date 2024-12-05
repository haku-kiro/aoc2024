open Helpers.Grid

let part01_match_func grid i j =
  let result = ref [] in
  let word_length = 4 in
  let l = word_length - 1 in
  result := !result @ [ get_up grid i j l ];
  result := !result @ [ get_up_right grid i j l ];
  result := !result @ [ get_right grid i j l ];
  result := !result @ [ get_down_right grid i j l ];
  result := !result @ [ get_down grid i j l ];
  result := !result @ [ get_down_left grid i j l ];
  result := !result @ [ get_left grid i j l ];
  result := !result @ [ get_up_left grid i j l ];
  !result |> List.filter (fun word -> word = "XMAS") |> List.length
;;

let part01 filename =
  let data = Helpers.File.read_file filename in
  let grid = make_grid data in
  let total = walk_grid grid 'X' part01_match_func in
  total
;;
