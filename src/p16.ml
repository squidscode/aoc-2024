(*
 *  Solves Advent of Code Problem 16:
 *  https://adventofcode.com/2024/day/16
 *)

open Position

type direction = Up | Down | Left | Right
let string_of_direction direction =
  match direction with
  | Right -> "Right"
  | Down -> "Down"
  | Left -> "Left"
  | Up -> "Up"

let direction_compare d d' =
  String.compare (d |> string_of_direction) (d' |> string_of_direction)

module Iscorepos = struct
  type t = int * position * direction

  let compare ((score1, p, d) : t) ((score2, p', d') : t) : int =
    let i = Int.compare score1 score2 in
    let p = Iposition.compare p p' in
    let d = direction_compare d d' in
    if i <> 0 then i else if p <> 0 then p else d
end

type score_position = Iscorepos.t

module ScorePositionSet = Set.Make (Iscorepos)

type score_position_set = ScorePositionSet.t

open Format

let rec print_score_positions (hp: score_position list): unit =
  match hp with
  | [(score, {x;y}, direction)] -> 
    printf "(%d, (%d,%d), %s) " score x y (string_of_direction direction);
  | (score, {x;y}, direction) :: xs -> 
    printf "(%d, (%d,%d), %s) " score x y (string_of_direction direction);
    print_score_positions xs
  | [] -> ()

let counter_clockwise (direction : direction) : direction =
  match direction with Up -> Left | Left -> Down | Down -> Right | Right -> Up

let clockwise (direction : direction) : direction =
  match direction with Up -> Right | Left -> Up | Down -> Left | Right -> Down

let nxt_position ({ x; y } : position) (direction : direction) : position =
  match direction with
  | Up -> { x; y = y - 1 }
  | Down -> { x; y = y + 1 }
  | Left -> { x = x - 1; y }
  | Right -> { x = x + 1; y }

(** Given a position map and a (score, position, direction) tuple, 
    return a set of all of the next positions *)
let expand (pmap : pmap) ((score, pos, dir) : score_position) :
    score_position_set =
  let clockwise = (score + 1000, pos, clockwise dir) in
  let counterclockwise = (score + 1000, pos, counter_clockwise dir) in
  let a = ScorePositionSet.singleton clockwise 
    |> ScorePositionSet.add counterclockwise in
  let nxt = nxt_position pos dir in
  if PositionMap.find_opt pos pmap = Some '#' then
    a
  else
    a |> ScorePositionSet.add (score + 1, nxt, dir)

let rec djikstras ?(seen = ScorePositionSet.empty) (pmap : pmap) (end_pos : position)
    (hp : score_position_set) : int =

  printf "seen sz: %d ; hp sz: %d\n" 
    (seen |> ScorePositionSet.cardinal) 
    (hp |> ScorePositionSet.cardinal);
  (*
  printf "\n";
  printf "seen:"; 
  print_score_positions (ScorePositionSet.to_list seen);
  printf "\n";
  printf "hp:"; 
  print_score_positions (ScorePositionSet.to_list hp);
  printf "\n";
  *)

  let ((score, pos, dir) as sp) = ScorePositionSet.min_elt hp in
  printf "processing: (%d, (%d,%d), %s)\n" score pos.x pos.y (string_of_direction dir);
  let hp = ScorePositionSet.remove sp hp
    |> ScorePositionSet.filter 
      (fun (_, pos', dir') -> not (pos = pos' && dir = dir'))
  in

  if pos = end_pos then (* base case / end condition *)
    score
  else if
    (* skip expansion if position is already in seen *)
    ScorePositionSet.find_opt (0,pos,dir) seen
      |> Option.is_some
  then
    djikstras ~seen pmap end_pos hp
  else (* expand all reachable from hp *)
    let new_seen = ScorePositionSet.add (0,pos,dir) seen in
    let reachable = expand pmap sp in
    let new_hp =
      ScorePositionSet.union reachable hp
      |> ScorePositionSet.filter 
        (fun (_, pos', dir') -> not (pos = pos' && dir = dir'))
      |> ScorePositionSet.filter
        (fun (_, pos', dir') -> 
          seen
          |> ScorePositionSet.find_opt (0, pos', dir')
          |> Option.is_none
        )
    in

    (*
    printf "reachable:"; 
    print_score_positions (ScorePositionSet.to_list reachable); 
    printf "\n";
    *)

    djikstras ~seen:new_seen pmap end_pos new_hp

let part_one (pmap : pmap) : int =
  let start_pos =
    pmap |> pmap_get_positions 'S' |> PositionSet.find_first (fun _ -> true)
  in
  let end_pos =
    pmap |> pmap_get_positions 'E' |> PositionSet.find_first (fun _ -> true)
  in
  print_pmap pmap;
  (0, start_pos, Right) |> ScorePositionSet.singleton |> djikstras pmap end_pos

module PositionDirection = struct
  type t = position * direction
  let compare ((p1, d1) : t) ((p2, d2) : t): int =
    let pc = Iposition.compare p1 p2 in
    let dc = String.compare (string_of_direction d1) (string_of_direction d2) in
    if pc <> 0 then pc else dc
end

module PDMap = Map.Make(PositionDirection);;

(* Position Direction -> (score, path) list *)
type pd_score_path = ((int * position list) list) PDMap.t;;

let path_compare (pth1 : position list) (pth2 : position list): int =
  let pthc = Int.compare (List.length pth1) (List.length pth2) in
  if pthc <> 0 then pthc else
  List.fold_left2 (fun (acc : int) (pl : position) (pr : position) -> 
    if acc <> 0 then acc else Iposition.compare pl pr
  ) 0 pth1 pth2

module SPDP = struct
  type t = (int * position * direction * position list)
  let compare ((s1, p1, d1, pth1) : t) ((s2, p2, d2, pth2) : t) : int =
    let i = Int.compare s1 s2 in
    let p = Iposition.compare p1 p2 in
    let d = direction_compare d1 d2 in
    let pthc = path_compare pth1 pth2 in
    if i <> 0 then i else 
    if p <> 0 then p else
    if d <> 0 then d else
    pthc
end

module SPDPSet = Set.Make(SPDP)
type spdp_set = SPDPSet.t

let rec djikstras2 ?(seen:pd_score_path = PDMap.empty) (pmap : pmap) 
  (end_pos : position) (hp : spdp_set) : (int * position list) list =

  printf "seen sz: %d ; hp sz: %d\n" 
    (seen |> PDMap.cardinal) 
    (hp |> SPDPSet.cardinal);

  let ((score, pos, dir, path) as sp) = SPDPSet.min_elt hp in
  printf "processing: (%d, (%d,%d), %s)\n" score pos.x pos.y (string_of_direction dir);
  let hp = SPDPSet.remove sp hp
    |> SPDPSet.filter 
      (fun (score', pos', dir', _) -> not 
        (pos = pos' && dir = dir' && score < score')
      )
  in

  if pos = end_pos then (* base case / end condition *)
    hp
      |> SPDPSet.filter (fun (score', pos', _, _) ->
          score = score' && pos = pos'
        )
      |> SPDPSet.to_list
      |> List.map (fun (s, _, _, p) -> (s, p))
      |> fun l -> (score, path) :: l
  else if
    (* skip expansion if position is already in seen *)
    PDMap.find_opt (pos, dir) seen
      |> fun opt -> match opt with
        | None -> false
        | Some spl -> spl |> 
          List.fold_left (fun b (score', _) -> 
            b || score' < score  (* skip if the score of any seen path is lower *)
          ) false
  then
    djikstras2 ~seen pmap end_pos hp
  else (* expand all reachable from hp *)
    let new_seen = PDMap.find_opt (pos,dir) seen
      |> Option.value ~default:[]
      |> fun l -> PDMap.add (pos,dir) ((score, path)::l) seen in
    let reachable = expand pmap (score, pos, dir) in
    let new_hp = reachable
      |> ScorePositionSet.to_list
      |> List.map (fun (score, position, direction) -> 
          (score, position, direction, position :: path)
        )
      |> SPDPSet.of_list
      |> SPDPSet.union hp
      |> SPDPSet.filter 
        (fun (score', pos', dir', _) -> not (pos = pos' && dir = dir' && score < score'))
    in

    djikstras2 ~seen:new_seen pmap end_pos new_hp

let part_two (pmap : pmap) : int = 
  let start_pos =
    pmap |> pmap_get_positions 'S' |> PositionSet.find_first (fun _ -> true)
  in
  let end_pos =
    pmap |> pmap_get_positions 'E' |> PositionSet.find_first (fun _ -> true)
  in
  print_pmap pmap;
  let ans: (int * position list) list = (0, start_pos, Right, [start_pos]) 
      |> SPDPSet.singleton 
      |> djikstras2 pmap end_pos in
  List.iter (fun (score, path) ->
    printf "SCORE: %d achieved via " score;
    List.iter (fun ({x;y} : position) -> printf "(%d,%d) -> " x y) (List.rev path);
    printf "\n"
  ) ans;
  ans
    |> List.map (fun (_, p) -> p)
    |> List.flatten
    |> PositionSet.of_list
    |> PositionSet.cardinal
  

let parse_file (file_content : string) : pmap =
  file_content |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s <> 0)
  |> get_pmap |> pmap_remove '.'
