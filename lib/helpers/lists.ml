open Core

let count_occurances input_list =
  let count_map = Map.empty (module Int) in
  List.fold_left input_list ~init:count_map ~f:(fun map element ->
    Map.update map element ~f:(function
      | None -> 1
      | Some count -> count + 1))
;;

let rec get_at_index lst idx =
  match lst, idx with
  | [], _ -> failwith "Index out of bounds"
  | x :: _, 0 -> x
  | _ :: xs, n when n > 0 -> get_at_index xs (n - 1)
  | _, _ -> failwith "Invalid index"
;;
