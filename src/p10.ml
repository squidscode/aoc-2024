(*
 *  Solves Advent of Code Problem 10:
 *  https://adventofcode.com/2024/day/10
 *)

open Format
module IntMap = Map.Make (Int)

module Iposition = struct
  type t = { x : int; y : int }

  let compare { x = x1; y = y1 } { x = x2; y = y2 } : int =
    let xc = Int.compare x1 x2 in
    if xc != 0 then xc else Int.compare y1 y2
end

type position = Iposition.t

module PositionSet = Set.Make (Iposition)
module BoolSet = Set.Make (Bool)

type position_set = PositionSet.t

let empty_position_set = PositionSet.empty

type height_map = position_set IntMap.t

let padd (p1 : position) (p2 : position) : position =
  { x = p1.x + p2.x; y = p1.y + p2.y }

let pdiff (p1 : position) (p2 : position) : position =
  { x = p1.x - p2.x; y = p1.y - p2.y }

let is_adj (p1 : position) (p2 : position) : bool =
  let diff = pdiff p1 p2 in
  let dsq = (diff.x * diff.x) + (diff.y * diff.y) in
  dsq = 1

let rec print_path (path : position list) : unit =
  match path with
  | [ { x; y } ] -> printf "(%d,%d)" x y
  | { x; y } :: l ->
      printf "(%d,%d)::" x y;
      print_path l
  | [] -> ()

let cint (c : char) : int = int_of_char c - int_of_char '0'

let print_position_set (pset : position_set) =
  pset |> PositionSet.iter (fun { x; y } -> printf "(%d,%d)," x y)

let int_map_get i (hmp : height_map) : position_set =
  Option.value ~default:empty_position_set (IntMap.find_opt i hmp)

let part_one (hmp : height_map) : int =
  let zeros = int_map_get 0 hmp in

  (* take one step *)
  let step (current_position : position) (next : int) : position_set =
    let possible_next_positions = int_map_get next hmp in
    let nxt =
      possible_next_positions
      |> PositionSet.filter (fun p1 -> is_adj current_position p1)
    in
    printf "step %d,%d %d = " current_position.x current_position.y next;
    print_position_set nxt;
    printf "\n";
    nxt
  in

  (* solve for all paths *)
  let solve (initial_paths : position_set) : position list list =
    List.init 9 (( + ) 1)
    |> List.fold_left
         (fun lpset i ->
           let new_paths =
             lpset
             |> List.map (fun path ->
                    let h = List.hd path in
                    let new_paths =
                      step h i |> PositionSet.to_seq
                      |> Seq.map (fun ps -> ps :: path)
                      |> List.of_seq
                    in
                    new_paths)
             |> List.flatten
           in
           printf "\nSOLVING FOR %d\n" i;
           List.iter
             (fun new_path ->
               print_path new_path;
               printf "\n")
             new_paths;
           new_paths)
         (List.map (fun x -> [ x ])
         @@ List.of_seq
         @@ PositionSet.to_seq initial_paths)
  in

  List.init 10 Fun.id
  |> List.iter (fun i ->
         printf "hmp[%i]: " i;
         print_position_set (int_map_get i hmp);
         printf "\n");

  solve zeros
  |> List.map (fun path ->
         let last = List.hd path in
         let first = List.nth path (List.length path - 1) in
         (first, last))
  |> List.sort_uniq (fun (p11, p12) (p21, p22) ->
         if Iposition.compare p11 p21 != 0 then
           Iposition.compare p11 p21
         else
           Iposition.compare p12 p22)
  |> List.length

let part_two (hmp : height_map) : int =
  let zeros = int_map_get 0 hmp in

  (* take one step *)
  let step (current_position : position) (next : int) : position_set =
    let possible_next_positions = int_map_get next hmp in
    let nxt =
      possible_next_positions
      |> PositionSet.filter (fun p1 -> is_adj current_position p1)
    in
    printf "step %d,%d %d = " current_position.x current_position.y next;
    print_position_set nxt;
    printf "\n";
    nxt
  in

  (* solve for all paths *)
  let solve (initial_paths : position_set) : position list list =
    List.init 9 (( + ) 1)
    |> List.fold_left
         (fun lpset i ->
           let new_paths =
             lpset
             |> List.map (fun path ->
                    let h = List.hd path in
                    let new_paths =
                      step h i |> PositionSet.to_seq
                      |> Seq.map (fun ps -> ps :: path)
                      |> List.of_seq
                    in
                    new_paths)
             |> List.flatten
           in
           printf "\nSOLVING FOR %d\n" i;
           List.iter
             (fun new_path ->
               print_path new_path;
               printf "\n")
             new_paths;
           new_paths)
         (List.map (fun x -> [ x ])
         @@ List.of_seq
         @@ PositionSet.to_seq initial_paths)
  in

  List.init 10 Fun.id
  |> List.iter (fun i ->
         printf "hmp[%i]: " i;
         print_position_set (int_map_get i hmp);
         printf "\n");

  solve zeros |> List.length

let parse_file (file_content : string) : height_map =
  file_content |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s <> 0)
  |> List.map String.to_seqi |> List.map List.of_seq
  |> List.mapi (fun y l -> l |> List.map (fun (x, c) -> ((x, y), cint c)))
  |> List.flatten
  |> List.fold_left
       (fun mp ((x, y), c) ->
         IntMap.add c
           (PositionSet.add { x; y }
              (Option.value ~default:empty_position_set (IntMap.find_opt c mp)))
           mp)
       IntMap.empty
