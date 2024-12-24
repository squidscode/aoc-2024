(*

   Solves Advent of Code Problem 1:
     https://adventofcode.com/2024/day/1
*)

let part_one (ins : (int * int) list) : int =
  let left = List.map (fun t -> match t with x, _ -> x) ins in
  let right = List.map (fun t -> match t with _, x -> x) ins in
  let left = List.sort Int.compare left in
  let right = List.sort Int.compare right in
  List.combine left right
  |> List.fold_left (fun acc (l, r) -> acc + abs (l - r)) 0

let part_two (ins : (int * int) list) : int =
  let rec add_to_map (mp : (int * int) list) (n : int) : (int * int) list =
    match mp with
    | (i, freq) :: rst ->
        if i = n then (i, freq + 1) :: rst else (i, freq) :: add_to_map rst n
    | [] -> [ (n, 1) ]
  in
  let left = List.map (fun t -> match t with x, _ -> x) ins in
  let right = List.map (fun t -> match t with _, x -> x) ins in
  let freq_map = List.fold_left add_to_map [] right in
  List.fold_left
    (fun acc n ->
      acc + (n * Option.value (List.assoc_opt n freq_map) ~default:0))
    0 left

let parse_file (file_content : string) =
  let int_pairs =
    file_content |> String.split_on_char '\n'
    |> List.map (Str.split (Str.regexp "[ \t]+"))
    |> List.map (List.filter (fun s -> not @@ String.equal "" s))
    |> List.map (List.map int_of_string)
    |> List.map (fun l ->
           assert (List.length l = 2 || List.length l = 0);
           l)
    |> List.filter (fun l -> List.length l = 2)
    |> List.map (fun l -> (List.hd l, List.hd (List.tl l)))
  in
  int_pairs
