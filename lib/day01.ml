let getLeftAndRight filename =
  let left, right =
    Helpers.File.read_file filename
    |> List.map (String.split_on_char ' ')
    |> List.map (function
      | [ x; _; _; y ] -> x, y
      | xs ->
        (*printing out the invalid elements for debugging*)
        Printf.printf "[%s]" (String.concat "; " xs);
        failwith "Invalid list length")
    |> List.split
  in
  left, right
;;

let part01 filename =
  let left, right = getLeftAndRight filename in
  let sortedLeft = List.map int_of_string left |> List.sort compare in
  let sortedRight = List.map int_of_string right |> List.sort compare in
  let total =
    List.fold_left2
      (fun total x y -> total + abs (x - y))
      0
      sortedLeft
      sortedRight
  in
  total
;;

let part02 filename =
  let left, right = getLeftAndRight filename in
  let count_map =
    Helpers.Lists.count_occurances (List.map int_of_string right)
  in
  let total =
    List.fold_left
      (fun total x -> total + (Helpers.Maps.map_value count_map x * x))
      0
      (List.map int_of_string left)
  in
  total
;;
