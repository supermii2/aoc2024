let rec is_safe_dir (lst: int list) (is_inc : bool) : bool =
  match lst with
  | [] -> true
  | [h1] -> true
  | h1 :: h2 :: rest ->
    if is_inc then
      let diff = h2 - h1 in
      diff >= 1 && diff <= 3 && is_safe_dir (h2 :: rest) is_inc
    else
      let diff = h1 - h2 in
      diff >= 1 && diff <= 3 && is_safe_dir (h2 :: rest) is_inc

let is_safe (lst : int list) : bool =
  match lst with
  | [] -> true
  | [h1] -> true
  | [h1; h2] ->
    let diff = abs (h2 - h1) in
    diff >= 1 && diff <= 3
  | h1 :: (h2 :: rest as tl) ->
    let diff = h2 - h1 in
    if diff > 3 || diff < -3 || diff == 0 then
      false
    else
      is_safe_dir tl (diff > 0)



let () =
  let channel = open_in "input" in
  let rec read_lines (acc: bool list) : bool list =
    try
      let a = is_safe (List.map (fun s -> int_of_string s) (String.split_on_char ' ' (input_line channel))) in
      read_lines (a :: acc)

    with End_of_file ->
      close_in channel;
      acc
  in
  let result = List.fold_left (fun acc b -> if b then acc + 1 else acc) 0 (read_lines []) in
  Printf.printf "Result: %d" result