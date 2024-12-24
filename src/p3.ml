(*

   Solves Advent of Code Problem 3:
     https://adventofcode.com/2024/day/3
*)

let part_one (l : (int * int) list) : int =
  List.fold_left (fun acc (l, r) -> acc + (l * r)) 0 l

type instruction = Mul of int * int | Do | Dont

let part_two (instructions : instruction list) : int =
  let acc_fn (sum, enabled) instruction =
    match instruction with
    | Mul (l, r) -> ((sum + if enabled then l * r else 0), enabled)
    | Do -> (sum, true)
    | Dont -> (sum, false)
  in
  let sum, _ = List.fold_left acc_fn (0, true) instructions in
  sum

let parse_file (file_content : string) : (int * int) list =
  let muls =
    file_content
    |> Re.all
         (Re.Perl.compile (Re.Perl.re {|mul\(([0-9]{1,3}),([0-9]{1,3})\)|}))
  in
  let l =
    muls
    |> List.map Re.Group.get (* partial function application *)
    |> List.map (fun f -> f 1) (* get the 0th group, i.e. the full match *)
    |> List.map int_of_string
  in
  let r =
    muls
    |> List.map Re.Group.get (* partial function application *)
    |> List.map (fun f -> f 2) (* get the 0th group, i.e. the full match *)
    |> List.map int_of_string
  in
  List.combine l r

let parse_file_two (file_content : string) : instruction list =
  let f gg : instruction =
    let full = gg 0 in
    match full with
    | "do()" -> Do
    | "don't()" -> Dont
    | _ -> Mul (int_of_string (gg 1), int_of_string (gg 2))
  in
  file_content
  |> Re.all
       (Re.Perl.compile
          (Re.Perl.re {|do\(\)|don't\(\)|mul\(([0-9]{1,3}),([0-9]{1,3})\)|}))
  |> List.map Re.Group.get (* partial function application *)
  |> List.map f
