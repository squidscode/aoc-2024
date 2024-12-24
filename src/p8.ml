(*
 *  Solves Advent of Code Problem 8:
 *  https://adventofcode.com/2024/day/8
 *)

type position = { x : int; y : int }
type frequency = { c : char; positions : position list }
type map = { width : int; height : int; frequencies : frequency list }

let pos_compare (p1 : position) (p2 : position) : int =
  let xc = Int.compare p1.x p2.x in
  let yc = Int.compare p1.y p2.y in
  if yc <> 0 then yc else xc

let print_positions (width : int) (height : int) (ps : position list) =
  List.init height Fun.id
  |> List.iter (fun y ->
         List.init width Fun.id
         |> List.iter (fun x ->
                Format.printf (if List.mem { x; y } ps then "#" else "."));
         Format.printf "\n")

let pos_diff (p1 : position) (p2 : position) : position =
  { x = p1.x - p2.x; y = p1.y - p2.y }

let pos_add (p1 : position) (p2 : position) : position =
  { x = p1.x + p2.x; y = p1.y + p2.y }

let get_antinodes (freq : frequency) : position list =
  let rec get_pairs ?(acc : (position * position) list = []) (l : position list)
      : (position * position) list =
    match l with
    | p1 :: xs -> get_pairs ~acc:(acc @ List.map (fun p2 -> (p1, p2)) xs) xs
    | [] -> acc
  in
  let pairs = get_pairs freq.positions in
  Format.printf "%d has %d pairs\n"
    (List.length freq.positions)
    (List.length pairs);
  pairs
  |> List.map (fun (p1, p2) ->
         let diff = pos_diff p1 p2 in
         [ pos_add p1 diff; pos_diff p2 diff ])
  |> List.flatten |> List.sort_uniq pos_compare

let part_one (m : map) : int =
  let clampx x = x |> Int.max 0 |> Int.min (m.width - 1) in
  let clampy y = y |> Int.max 0 |> Int.min (m.height - 1) in
  let antinodes =
    m.frequencies |> List.map get_antinodes |> List.flatten
    |> List.sort_uniq pos_compare
    |> List.filter (fun { x; y } -> clampx x = x && clampy y = y)
  in
  print_positions m.width m.height antinodes;
  List.length antinodes

let get_antinodes_in_line (oob : position -> bool) (freq : frequency) :
    position list =
  let rec get_pairs ?(acc : (position * position) list = []) (l : position list)
      : (position * position) list =
    match l with
    | p1 :: xs -> get_pairs ~acc:(acc @ List.map (fun p2 -> (p1, p2)) xs) xs
    | [] -> acc
  in
  let rec keep_add ?(acc : position list = []) (p1 : position) (p2 : position) =
    if oob p1 then acc else keep_add ~acc:(p1 :: acc) (pos_add p1 p2) p2
  in
  let rec keep_diff ?(acc : position list = []) (p1 : position) (p2 : position)
      =
    if oob p1 then acc else keep_diff ~acc:(p1 :: acc) (pos_diff p1 p2) p2
  in
  let pairs = get_pairs freq.positions in
  Format.printf "%d has %d pairs\n"
    (List.length freq.positions)
    (List.length pairs);
  pairs
  |> List.map (fun (p1, p2) ->
         let diff = pos_diff p1 p2 in
         keep_add p1 diff @ keep_diff p2 diff)
  |> List.flatten |> List.sort_uniq pos_compare

let part_two (m : map) : int =
  let clampx x = x |> Int.max 0 |> Int.min (m.width - 1) in
  let clampy y = y |> Int.max 0 |> Int.min (m.height - 1) in
  let oob { x; y } = clampx x <> x || clampy y <> y in
  let antinodes =
    m.frequencies
    |> List.map (get_antinodes_in_line oob)
    |> List.flatten |> List.sort_uniq pos_compare
    |> List.filter (fun { x; y } -> clampx x = x && clampy y = y)
  in
  print_positions m.width m.height antinodes;
  List.length antinodes

let parse_file (file_content : string) : map =
  let lines =
    file_content |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length s <> 0)
  in
  let antenna_locations =
    lines
    |> List.mapi (fun y s ->
           Seq.map (fun (x, c) -> ((x, y), c)) (String.to_seqi s))
    |> List.map List.of_seq |> List.flatten
    |> List.filter (fun (_, c) -> c <> '.')
  in
  let frequency_chars =
    antenna_locations
    |> List.map (fun (_, c) -> c)
    |> List.sort_uniq Char.compare
  in
  let frequencies =
    frequency_chars
    |> List.map (fun c ->
           {
             c;
             positions =
               antenna_locations
               |> List.filter (fun (_, c') -> c' = c)
               |> List.map (fun ((x, y), _) -> { x; y });
           })
  in
  frequencies
  |> List.iter (fun { c; positions } ->
         Format.printf "%c: " c;
         List.iter (fun { x; y } -> Format.printf "(%d, %d) " x y) positions;
         Format.printf "\n");
  {
    width = List.hd lines |> String.length;
    height = List.length lines;
    frequencies;
  }
