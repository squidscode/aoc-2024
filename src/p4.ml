(*
 *  Solves Advent of Code Problem 4:
 *  https://adventofcode.com/2024/day/4
 *)

(* Top-left-to-bottom-right diagonals *)
let tlbr_diagonal (l : string list) : string list =
  let nx = String.length (List.hd l) in
  let ny = List.length l in
  let rec diagonal acc x y =
    if x < nx && y < ny then
      diagonal
        (acc ^ String.make 1 (String.get (List.nth l y) x))
        (x + 1) (y + 1)
    else
      acc
  in
  let left_side = List.mapi (fun i _ -> diagonal "" 0 i) l in
  let top_side =
    List.of_seq
      (Seq.mapi (fun i _ -> diagonal "" i 0) (String.to_seq (List.hd l)))
  in
  [ diagonal "" 0 0 ] @ List.tl left_side @ List.tl top_side

let bltr_diagonal (l : string list) : string list =
  let nx = String.length (List.hd l) in
  let ny = List.length l in
  let rec diagonal acc x y =
    if x < nx && 0 <= y then
      diagonal
        (acc ^ String.make 1 (String.get (List.nth l y) x))
        (x + 1) (y - 1)
    else
      acc
  in
  let left_side = List.mapi (fun i _ -> diagonal "" 0 (ny - i - 1)) l in
  let bottom_side =
    List.of_seq
      (Seq.mapi (fun i _ -> diagonal "" i (ny - 1)) (String.to_seq (List.hd l)))
  in
  [ diagonal "" 0 (ny - 1) ] @ List.tl left_side @ List.tl bottom_side

let vertical (l : string list) : string list =
  let rec get_vertical i acc y =
    if y < List.length l then
      get_vertical i (acc ^ String.make 1 (String.get (List.nth l y) i)) (y + 1)
    else
      acc
  in
  List.hd l |> String.to_seq
  |> Seq.mapi (fun i _ -> get_vertical i "" 0)
  |> List.of_seq

(* Why am I forced to write this *)
let str_rev (s : string) : string =
  s |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq

let count_xmas (s : string) : int =
  let re = Re.compile (Re.Perl.re "XMAS") in
  s |> Re.all re |> List.length

let part_one (l : string list) : int =
  let tlbr = tlbr_diagonal l in
  let brtl = tlbr |> List.map str_rev in
  let bltr = bltr_diagonal l in
  let trbl = bltr |> List.map str_rev in
  let vert = vertical l in
  let trev = vert |> List.map str_rev in
  let llll = l in
  let lrev = llll |> List.map str_rev in
  let alll = tlbr @ brtl @ bltr @ trbl @ vert @ trev @ llll @ lrev in
  (*
  let print l s =
    Format.printf "%s\n" s;
    List.iter (Format.printf "%s\n") l
  in
  print tlbr "tlbr";
  print brtl "brtl";
  print bltr "bltr";
  print trbl "trbl";
  print vert "vert";
  print trev "trev";
  print llll "llll";
  print lrev "lrev";
  *)
  alll |> List.map count_xmas |> List.fold_left ( + ) 0

let part_two (l : string list) : int =
  let nx = String.length (List.hd l) in
  let ny = List.length l in
  let coords =
    l
    |> List.mapi (fun y s -> (y, s))
    |> List.map (fun (y, s) ->
           List.map (fun (x, c) -> ((x, y), c)) (List.of_seq (String.to_seqi s)))
    |> List.flatten
  in
  let a_coords =
    coords
    |> List.filter (fun (_, c) -> c = 'A')
    |> List.filter (fun ((x, y), _) ->
           1 <= x && x <= nx - 2 && 1 <= y && y <= ny - 2)
  in
  let xmas's =
    a_coords
    |> List.filter (fun ((x, y), _) ->
           let tl = List.assoc (x - 1, y - 1) coords in
           let tr = List.assoc (x + 1, y - 1) coords in
           let bl = List.assoc (x - 1, y + 1) coords in
           let br = List.assoc (x + 1, y + 1) coords in
           List.sort Char.compare [ tl; br ] = [ 'M'; 'S' ]
           && List.sort Char.compare [ tr; bl ] = [ 'M'; 'S' ])
  in
  List.length xmas's

let parse_file (file_content : string) : string list =
  file_content |> String.split_on_char '\n'
  |> List.filter (fun s -> not (String.equal s ""))
