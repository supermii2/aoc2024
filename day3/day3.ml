#load "str.cma";;
open Str;;

let parse filename =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let s = input_line channel in
      read_lines (s :: acc)

    with End_of_file ->
      close_in channel;
      (acc)
  in
  read_lines []

let sum_mul_expressions input =
  let regex = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec process acc start =
    try
      let _ = Str.search_forward regex input start in
      let x = int_of_string (Str.matched_group 1 input) in
      let y = int_of_string (Str.matched_group 2 input) in
      let product = x * y in
      process (acc + product) (Str.match_end ())
  with Not_found->
    acc

  in
  process 0 0

let () =
  let x = parse "input" in
  let z = List.map (fun y -> sum_mul_expressions y) x in
  let result = List.fold_left(fun acc q -> acc + q) 0 z in
  Printf.printf "Result: %d" result