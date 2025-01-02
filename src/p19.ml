(* 
 *  Solves Advent of Code Problem 19:
 *  https://adventofcode.com/2024/day/19
 *)

exception ParseError of string


(*** DATA DEFINITIONS ***)
type color = White | Blue | Black | Red | Green
type towel_pattern = color list
type towel_patterns = towel_pattern list
type problem = {
  available_towel_patterns : towel_patterns; 
  desired_designs : towel_patterns 
}


(***  FORMATTING FUNCTIONS  ***)
let ctoch (color : color) : char =
  match color with
  | White -> 'w'
  | Blue -> 'u'
  | Black -> 'b'
  | Red -> 'r'
  | Green -> 'g'
let chtoc (c: char): color =
  match c with
  | 'w' -> White
  | 'u' -> Blue
  | 'b' -> Black
  | 'r' -> Red
  | 'g' -> Green
| _ -> raise (ParseError "Invalid char in chtoc")
let tptos (towel_pattern : towel_pattern) : string =
  towel_pattern
    |> List.to_seq
    |> Seq.map ctoch
    |> String.of_seq
let stotp (str : string) : towel_pattern =
  str
    |> String.to_seq
    |> Seq.map chtoc
    |> List.of_seq


(*******      LOGIC     *******)
module Itp = struct
  type t = towel_pattern
  let equal (tp1 : t) (tp2 : t) : bool =
    tp1 = tp2
  let hash (tp : t) : int =
    String.hash (tptos tp)
end
module ItpTbl = Hashtbl.Make(Itp)

let rec prefix_for (target : towel_pattern) (pre : towel_pattern) : bool =
  match (target, pre) with
  | (_, []) -> true
  | (c1 :: rst1, c2 :: rst2) -> c1 = c2 && prefix_for rst1 rst2
  | _ -> false
let without_prefix (target : towel_pattern) (pre : towel_pattern) : towel_pattern =
  target
    |> List.to_seq
    |> Seq.drop (List.length pre)
    |> List.of_seq
let rec satisfiable ?(tbl : int ItpTbl.t = ItpTbl.create 10_000)
    (av : towel_patterns) (target : towel_pattern) : int =
  match target with
  | [] -> 1
  | _ -> 
    if ItpTbl.mem tbl target then
      ItpTbl.find tbl target
    else
    let ans = av
      |> List.filter (prefix_for target)
      |> List.fold_left (fun b pre -> 
          b + satisfiable ~tbl av (without_prefix target pre)
        ) 0 in
    ItpTbl.add tbl target ans;
    ans
let part_one ({available_towel_patterns=av;desired_designs=des} : problem) : int = 
  Format.printf "des sz: %d\n" (List.length des);
  des |> List.mapi (fun ind d ->
      Format.printf "%d / %d :: " (ind + 1) (List.length des);
      Format.printf "satisfiable %s\n" (tptos d);
      Format.print_flush ();
      satisfiable av d
  )   |> List.map (fun b -> if b <> 0 then 1 else 0)
      |> List.fold_left (+) 0
let part_two ({available_towel_patterns=av;desired_designs=des} : problem) : int = 
  Format.printf "des sz: %d\n" (List.length des);
  des |> List.mapi (fun ind d ->
      Format.printf "%d / %d :: " (ind + 1) (List.length des);
      Format.printf "satisfiable %s\n" (tptos d);
      Format.print_flush ();
      satisfiable av d
  )
      |> List.fold_left (+) 0
let parse_file (file_content : string) : problem =
  let lines = file_content 
  |> String.split_on_char '\n'
  |> List.to_seq in
  let av = lines |> Seq.take 1 |> Seq.uncons |> Option.get |> fst 
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.map stotp
  in
  let des = lines |> Seq.drop 2 |> Seq.filter ((<>) "")
      |> Seq.map stotp |> List.of_seq in
  { available_towel_patterns=av ; desired_designs=des }
  
