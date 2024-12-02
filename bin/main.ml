let () = print_endline "AOC 2024, in OCaml"

let () =
  let total = Aoc2024.Day01.part01 "./data/day01.txt" in
  Printf.printf "Day 01, Part 01, Total: %s\n" (string_of_int total)
;;

let () =
  let total = Aoc2024.Day01.part02 "./data/day01.txt" in
  Printf.printf "Day 01, Part 02, Total: %s\n" (string_of_int total)
;;

let () =
  let total = Aoc2024.Day02.part01 "./data/day02.txt" in
  Printf.printf "Day 02, Part 01, Total: %s\n" (string_of_int total)
;;

(* was getting odd issues with rendering plots using owl_plplot, will get
   back to this when I have time *)
(*let () = Aoc2024.Day01_visualize.plot*)
