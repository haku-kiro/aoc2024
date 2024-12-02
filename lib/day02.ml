(* This checks that a current element (x) is within the bounds of the next
   element (y) *)
let rec in_bound bound = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) ->
    let check = abs (x - y) in
    if check > bound || check <= 0 then false else in_bound bound rest
;;

(* checks that the passed in list is in ascending order *)
let rec ascending = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) -> if x <= y then ascending rest else false
;;

(* checks that the passed in list is in descending order *)
let rec descending = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) -> if x >= y then descending rest else false
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

(* generates a list of lists where one element is removed each time *)
let skip lst =
  List.mapi (fun i _ -> List.filteri (fun j _ -> i <> j) lst) lst
;;

(* runs a bounds check and both an ascending or descending check *)
let checks lsts =
  let result =
    lsts
    |> List.map (fun x ->
      let asc_check = ascending x in
      let desc_check = descending x in
      let bound_check = (in_bound 3) x in
      if bound_check && (asc_check || desc_check) then true else false)
  in
  result
;;

let part02 filename =
  let total =
    Helpers.File.read_file filename
    |> List.map (String.split_on_char ' ')
    |> List.map (List.map int_of_string)
    |> List.map skip
    |> List.map checks
    |> List.fold_left
         (fun count x ->
           let valid = List.exists (fun x -> x) x in
           if valid then count + 1 else count)
         0
  in
  total
;;
