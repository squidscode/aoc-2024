(*
 *  Solves Advent of Code Problem 11:
 *  https://adventofcode.com/2024/day/11
 *)

let times : int = 25
let part_one (_ : int list) : int = 0
let part_two (_ : int list) : int = 0 (* DELETE ME *)

let parse_file (file_content : string) : int list =
  file_content |> String.split_on_char '\n' |> List.hd
  |> String.split_on_char ' '
  |> List.filter (fun s -> String.length s <> 0)
  |> List.map int_of_string
