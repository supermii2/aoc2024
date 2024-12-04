#load "str.cma";;
open Str;;

let parse filename =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let s = input_line channel in
      read_lines acc^s

    with End_of_file ->
      close_in channel;
      (acc)
  in
  read_lines ""

let sum_mul_expressions input =
  let regex = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|don't()|} in
  let regex2 = Str.regexp {|do()|} in
  let rec process acc start is_do =
    if is_do then
      try
        let _ = Str.search_forward regex input start in
        if ((Str.matched_string input) = "don't()") then
          process acc (Str.match_end ()) false
        else
          let x = int_of_string (Str.matched_group 1 input) in
          let y = int_of_string (Str.matched_group 2 input) in
          let product = x * y in
          process (acc + product) (Str.match_end ()) true
      with Not_found->
        acc
    else
      try
        let _ = Str.search_forward regex2 input start in
        process acc (Str.match_end ()) true
      with Not_found ->
        acc
    in
  process 0 0 true

let () =
  let x = parse "input" in
  let result = sum_mul_expressions x in
  Printf.printf "Result: %d" result