#! /usr/bin/env bash -eu

n=$1
file="src/p${n}.ml"

test ! -e $file || (echo "$file already exists!" >&2 && exit 1)
cat > $file <<EOF
(* 
 *  Solves Advent of Code Problem ${n}:
 *  https://adventofcode.com/2024/day/${n}
 *)

let part_one (_: string list): int =
  0 (* DELETE ME *)
;;

let part_two (_: string list): int =
  0 (* DELETE ME *)
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

main="./src/main.ml"
search_str="    \(\* TEMPLATE.SH WILL INSERT HERE \*\)"
prepend="    | \"${n}.1\" -> s |> P${n}.parse_file |> P${n}.part_one\n    | \"${n}.2\" -> s |> P${n}.parse_file |> P${n}.part_two\n\n    (* TEMPLATE.SH WILL INSERT HERE *)"

perl -pi -e "s#${search_str}#${prepend}#g" "${main}"

./get_input.py "${n}" -o "./resources/${n}.txt"

dune fmt
