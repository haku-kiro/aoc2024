open Core

let get_rules_and_updates lst =
  let rules = List.filter ~f:(fun str -> String.contains str '|') lst in
  let updates = List.filter ~f:(fun str -> String.contains str ',') lst in
  rules, updates
;;

let get_ordering_rule lst =
  match lst with [ a; b ] -> int_of_string a, int_of_string b | _ -> 0, 0
;;

let create_rule_map lst =
  let rule_map = Map.empty (module Int) in
  (* each element would be 'n|n' *)
  List.fold_left lst ~init:rule_map ~f:(fun map element ->
    let parts = String.split ~on:'|' element in
    let left, right = get_ordering_rule parts in
    (* Will this not have duplicates in the list itself? Is that an issue? *)
    Map.update map left ~f:(function
      | None -> [ right ]
      | Some current -> current @ [ right ]))
;;

let create_update_lists lst =
  (* elements are; n,n,n,n *)
  lst
  |> List.map ~f:(fun line ->
    String.split ~on:',' line |> List.map ~f:(fun n -> int_of_string n))
;;

let rec order_check left right =
  match left, right with
  | lh :: lrest, rh :: rrest ->
    if lh = rh then true else order_check lrest right
  | [], right -> ()
;;

let valid_update rule_map update =
  (* rule, k:[v] *)
  (* update, [u] *)
  let update_checks =
    update
    |> List.mapi (fun idx elm ->
      let rule_list = Map.find rule_map elm |> Option.value in
      let update_list = List.take (idx + 1) update in
      0)
  in
  true
;;

let check_valid_updates rule_map updates =
  let _valid_updates = updates |> List.filter ~f:(valid_update rule_map) in
  ()
;;

let part01 filename =
  let data = Helpers.File.read_file filename in
  let rules, updates = get_rules_and_updates data in
  let rule_map = create_rule_map rules in
  let updates = create_update_lists updates in
  let _valid_updates = check_valid_updates rule_map updates in
  42
;;
