(* 

  Solves Advent of Code Problem 2:
    https://adventofcode.com/2024/day/2

 *)

let is_safe (report: int list): bool =
  let rec diff (l: int list) (s: int): int list =
    begin match l with
    | fst :: rst -> (fst - s) :: (diff rst fst)
    | [] -> []
    end
  in
  if List.length report <= 1 then true else
    let report_diff = diff report (List.hd report) in
    List.for_all (fun i -> 1 <= i && i <= 3) (List.tl report_diff)
    || List.for_all (fun i -> i <= -1 && i >= -3) (List.tl report_diff)
;;

let part_one (reports: int list list): int =
  List.fold_left (fun acc report -> acc + if is_safe report then 1 else 0) 0 reports
;;

(* Problem Dampener involves removing one of the reports to make it safe,
   this can be easily solved via brute force! *)
let part_two (reports: int list list): int =
  let rec permutations_one_removed (report: int list): int list list =
    match report with 
    | f :: r -> 
        let with_first = permutations_one_removed r
          |> List.map (fun l -> f :: l)
        in r :: with_first
    | [] -> []
  in
  List.fold_left 
    (fun acc report -> acc + 
      if is_safe report then 1 else 
      if List.exists is_safe (permutations_one_removed report) then 1 else 0)
    0 reports
;;

let parse_file (file_content: string): int list list =
  let reports = file_content 
    |> String.split_on_char '\n'
    |> List.map (Str.split (Str.regexp "[ \t]+"))
    |> List.map (List.filter (fun s -> not @@ String.equal "" s))
    |> List.map (List.map int_of_string)
    |> List.filter (fun l -> List.length l >= 1)
  in
  reports
;;
