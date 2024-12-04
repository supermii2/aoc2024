let read_grid filename =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  loop []

let directions = [ (0, 1); (1, 0); (1, 1); (1, -1); (0, -1); (-1, 0); (-1, -1); (-1, 1) ]

let check_word grid word x y dx dy =
  let rec aux i xi yi =
    if i = String.length word then true
    else
      let valid_position =
        xi >= 0 && yi >= 0 && xi < List.length grid && yi < String.length (List.nth grid xi)
      in
      if not valid_position || (List.nth grid xi).[yi] <> word.[i] then false
      else aux (i + 1) (xi + dx) (yi + dy)
  in
  aux 0 x y

let count_word grid word =
  let count = ref 0 in
  List.iteri
    (fun x row ->
      String.iteri
        (fun y _ ->
          List.iter
            (fun (dx, dy) ->
              if check_word grid word x y dx dy then incr count)
            directions)
        row)
    grid;
  !count

let () =
  let filename = "input" in
  let grid = read_grid filename in
  let word = "XMAS" in
  let result = count_word grid word in
  Printf.printf "Result: %d" result
