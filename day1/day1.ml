let parse filename =
  let channel = open_in filename in
  let rec read_lines acc1 acc2 =
    try
      let (a1, a2) = Scanf.sscanf (input_line channel) "%d %d" (fun x y -> x,y) in
      read_lines (a1 :: acc1) (a2 :: acc2)

    with End_of_file ->
      close_in channel;
      (acc1, acc2)
  in
  read_lines [] []
  
let () =
  let (a1, a2) = parse "input" in
  let sa1 = List.sort compare a1 in
  let sa2 = List.sort compare a2 in
  let d = List.map2 (fun x y -> abs (x - y)) sa1 sa2 in
  let sum = List.fold_left (+) 0 d in

  Printf.printf "Result: %d" sum