open Core

let count_occurances input_list =
  let count_map = Map.empty (module Int) in
  List.fold_left input_list ~init:count_map ~f:(fun map element ->
    Map.update map element ~f:(function
      | None -> 1
      | Some count -> count + 1))
;;
