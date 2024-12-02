let is_valid a b is_inc =
  if is_inc then
    let r = b - a in
    r >= 1 && r <= 3
  else
    let r = a - b in
    r >= 1 && r <= 3

let rec is_safe_dir (lst: int list) (is_inc : bool) (tolerance : int) : bool =
  if tolerance == 0 then
    match lst with
    | [] -> true
    | [h1] -> true
    | h1 :: h2 :: rest ->
      (is_valid h1 h2 is_inc) && (is_safe_dir (h2 :: rest) is_inc 0)
  else
    match lst with
    | [] -> true
    | [h1] -> true
    | [h1; h2] -> is_valid h1 h2 is_inc
    | h1 :: h2 :: h3 :: rest ->
      if (is_valid h1 h2 is_inc) && (is_valid h2 h3 is_inc) then
        is_safe_dir (h2 :: h3 :: rest) is_inc tolerance
      else
        if not (is_valid h1 h2 is_inc) then
          is_safe_dir (h2 :: h3 :: rest) is_inc (tolerance - 1) || is_safe_dir (h1 :: h3 :: rest) is_inc (tolerance - 1)
        else
          is_safe_dir (h1 :: h2 :: rest) is_inc (tolerance - 1) || is_safe_dir (h1 :: h3 :: rest) is_inc (tolerance - 1)

let find_dir lst =
  match lst with
  | h1 :: h2 :: h3 :: h4 :: rest ->
    let s1 = if h1 > h2 then 1 else if h1 < h2 then -1 else 0 in
    let s2 = if h1 > h3 then 1 else if h1 < h3 then -1 else 0 in
    let s3 = if h2 > h3 then 1 else if h2 < h3 then -1 else 0 in
    let s4 = if h2 > h4 then 1 else if h2 < h4 then -1 else 0 in
    let s5 = if h3 > h4 then 1 else if h3 < h4 then -1 else 0 in
    let s = s1 + s2 + s3 + s4 + s5 in
    if s > 0 then -1 else if s < 0 then 1 else 0
  | _ -> failwith "Oof"

let rec is_safe (lst : int list): bool =
  match lst with
  | [] -> true
  | [h1] -> true
  | [h1; h2] ->
    let diff = abs (h2 - h1) in
    diff >= 1 && diff <= 3
  | [h1; h2; h3] ->
    is_safe [h1; h3] || is_safe [h1; h2] || is_safe [h2; h3]
  | h1 :: h2 :: h3 :: h4 :: rest ->
    let dir = find_dir [h1; h2; h3; h4] in
    if dir == 0 then
      false
    else
      is_safe_dir lst (dir > 0) 1

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