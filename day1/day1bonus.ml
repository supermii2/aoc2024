let hashtbl_size = 100000

let parse (filename: string) : (int, int * int) Hashtbl.t =
  let channel = open_in filename in
  let rec read_lines (ht: (int, int * int) Hashtbl.t) : (int, int * int) Hashtbl.t =
    try
      let (a1, a2) = Scanf.sscanf (input_line channel) "%d %d" (fun x y -> x,y) in
      if Hashtbl.mem ht a1 then (
        let (x, y) = Hashtbl.find ht a1 in
        Hashtbl.replace ht a1 (x + 1, y)
      ) else (
        Hashtbl.add ht a1 (1, 0)
      );
      if Hashtbl.mem ht a2 then (
        let (x, y) = Hashtbl.find ht a2 in
        Hashtbl.replace ht a2 (x, y + 1)
      ) else (
        Hashtbl.add ht a2 (0, 1)
      );
      read_lines ht
    with End_of_file ->
      close_in channel;
      ht
    in
    let ht = Hashtbl.create hashtbl_size in
    read_lines ht

let get_sum ht =
  let sum = ref 0 in
  Hashtbl.iter (fun key (x, y) ->
    sum := !sum + (key * x * y)
  ) ht;
  !sum

let () =
  let ht = parse "input" in
  let result = get_sum ht in
  Printf.printf "Result: %d" result