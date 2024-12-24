(* 
 *  Solves Advent of Code Problem 19:
 *  https://adventofcode.com/2024/day/19
 *)

let part_one (_ : string list) : int = 0 (* DELETE ME *)
let part_two (_ : string list) : int = 0 (* DELETE ME *)

let parse_file (file_content : string) : string list =
  file_content |> String.split_on_char '\n'
(* split by whitespace
   |> List.map (Str.split (Str.regexp "[ \t]+"))
   |> List.map (List.filter (fun s -> not @@ String.equal "" s))
*)
