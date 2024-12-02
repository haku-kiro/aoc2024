let rec in_bound bound = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) ->
    let check = abs (x - y) in
    if check > bound || check <= 0 then false else in_bound bound rest
;;

let rec ascending = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) -> if x < y then ascending rest else false
;;

let rec descending = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) -> if x > y then descending rest else false
;;

let part01 filename =
  let total =
    Helpers.File.read_file filename
    |> List.map (String.split_on_char ' ')
    |> List.map (List.map int_of_string)
    |> List.filter (fun x -> ascending x || descending x)
    |> List.map (in_bound 3)
    |> List.fold_left (fun count x -> if x then count + 1 else count) 0
  in
  total
;;
