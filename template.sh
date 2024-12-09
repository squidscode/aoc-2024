#! /usr/bin/env bash -eu

n=$1
file="src/p${n}.ml"

test ! -e $file || (echo "$file already exists!" >&2 && exit 1)
cat > $file <<EOF
(* 
 *  Solves Advent of Code Problem ${n}:
 *  https://adventofcode.com/2024/day/${n}
 *)

let part_one (l: string list): int =
  assert (List.length l >= 0); 0 (* DELETE ME *)
;;

let part_two (l: string list): int =
  assert (List.length l >= 0); 0 (* DELETE ME *)
;;

let parse_file (file_content: string): string list =
  file_content
    |> String.split_on_char '\n'
  (* split by whitespace
    |> List.map (Str.split (Str.regexp "[ \t]+"))
    |> List.map (List.filter (fun s -> not @@ String.equal "" s))
  *)
;;
EOF
