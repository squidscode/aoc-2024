(*
 *  Solves Advent of Code Problem 12:
 *  https://adventofcode.com/2024/day/12
 *)

open Format

module Iposition = struct
  type t = { x : int; y : int }

  let compare { x = x1; y = y1 } { x = x2; y = y2 } : int =
    let xc = Int.compare x1 x2 in
    if xc != 0 then xc else Int.compare y1 y2
end

type position = Iposition.t

module PositionSet = Set.Make (Iposition)

type position_set = PositionSet.t

module CharMap = Map.Make (Char)

type c_to_pos = position_set CharMap.t

let mt_c_to_pos : c_to_pos = CharMap.empty
let mt_position_set : position_set = PositionSet.empty

let pdiff (p1 : position) (p2 : position) : position =
  { x = p1.x - p2.x; y = p1.y - p2.y }

let padj (p1 : position) (p2 : position) : bool =
  let d = pdiff p1 p2 in
  (d.x * d.x) + (d.y * d.y) = 1

let print_region (region : position_set) : unit =
  region |> PositionSet.iter (fun p -> printf "(%d, %d); " p.x p.y)

let print_regions (regions : position_set list) : unit =
  regions
  |> List.iteri (fun i region ->
         printf "region #%d: " i;
         print_region region;
         printf "\n")

let rec filter_region ?(seen : position_set = mt_position_set) (p : position)
    (position_set : position_set) : position_set =
  (*
   * printf "inner: filter_region (%d, %d) " p.x p.y;
   * print_region position_set; printf "\n";
   *)
  printf "(%d, %d), sz: %d\n" p.x p.y (PositionSet.cardinal position_set);
  let adjs =
    PositionSet.filter (padj p) position_set
    |> PositionSet.filter (Fun.negate (Fun.flip PositionSet.mem seen))
  in
  (* If adjs has more than one element, then we  *)
  match PositionSet.cardinal adjs with
  | 0 -> PositionSet.add p mt_position_set
  | _ ->
      let new_seen = PositionSet.union seen adjs |> PositionSet.add p in
      let region =
        adjs |> PositionSet.to_seq
        |> Seq.fold_left
             (fun set r ->
               let new_seen = PositionSet.union set new_seen in
               PositionSet.union set
                 (filter_region ~seen:new_seen r position_set))
             seen
      in
      region |> PositionSet.union new_seen

let rec get_regions' (positions : position_set) : position_set list =
  match PositionSet.cardinal positions with
  | 0 -> []
  | _ -> (
      let some_position = PositionSet.find_first (fun _ -> true) positions in
      let one_region = filter_region some_position positions in
      printf "filter_region (%d, %d) " some_position.x some_position.y;
      print_region positions;
      printf "\n ==> ";
      print_region one_region;
      printf "\n";
      let rest =
        PositionSet.filter
          (Fun.negate (Fun.flip PositionSet.mem one_region))
          positions
      in
      match PositionSet.cardinal rest with
      | x when x = PositionSet.cardinal positions -> [ one_region ]
      | _ -> one_region :: get_regions' rest)

let get_regions (mp : c_to_pos) : position_set list =
  mp |> CharMap.to_seq
  |> Seq.fold_left (fun l (_, positions) -> get_regions' positions :: l) []
  |> List.flatten
  |> List.filter (fun s -> PositionSet.cardinal s <> 0)

let get_adjs (p : position) : position list =
  [
    { x = p.x + 1; y = p.y };
    { x = p.x - 1; y = p.y };
    { x = p.x; y = p.y + 1 };
    { x = p.x; y = p.y - 1 };
  ]

let area (region : position_set) : int = PositionSet.cardinal region

let perimeter (region : position_set) : int =
  region |> PositionSet.to_seq |> List.of_seq |> List.map get_adjs
  |> List.flatten
  |> List.filter (fun p -> not (PositionSet.mem p region))
  |> List.length

let region_to_price (region : position_set) : int =
  let a = area region in
  let p = perimeter region in
  print_region region;
  printf "\nAREA: %d" a;
  printf "\nPERIMETER: %d\n" p;
  a * p

let part_one (mp : c_to_pos) : int =
  let regions = get_regions mp in
  print_regions regions;
  regions |> List.map region_to_price |> List.fold_left ( + ) 0

let get_sides (region : position_set) : int =
  region |> PositionSet.to_seq |> List.of_seq |> List.map get_adjs
  |> List.flatten
  |> List.filter (fun p -> not (PositionSet.mem p region))
  |> List.length

let get_all_adjs (adj_fn : position -> position) (region : position_set) :
    position_set =
  region |> PositionSet.to_seq |> List.of_seq |> List.map adj_fn |> List.to_seq
  |> PositionSet.of_seq
  |> PositionSet.filter (Fun.negate (Fun.flip PositionSet.mem region))

let padd (p1 : position) (p2 : position) : position =
  { x = p1.x + p2.x; y = p1.y + p2.y }

let sides (region : position_set) : int =
  let up = padd { x = 0; y = -1 } in
  let right = padd { x = 1; y = 0 } in
  let down = padd { x = 0; y = 1 } in
  let left = padd { x = -1; y = 0 } in
  [ up; right; down; left ]
  |> List.fold_left
       (fun sum fn -> sum + List.length (get_regions' (get_all_adjs fn region)))
       0

let region_to_price_with_discount (region : position_set) : int =
  let a = area region in
  let p = sides region in
  print_region region;
  printf "\nAREA: %d" a;
  printf "\nSIDES: %d\n" p;
  a * p

let part_two (mp : c_to_pos) : int =
  let regions = get_regions mp in
  print_regions regions;
  regions |> List.map region_to_price_with_discount |> List.fold_left ( + ) 0

let parse_file (file_content : string) : c_to_pos =
  file_content |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s <> 0)
  |> List.map String.to_seqi |> List.map List.of_seq
  |> List.mapi (fun y l -> List.map (fun (x, c) -> ((x, y), c)) l)
  |> List.flatten
  |> List.fold_left
       (fun mp ((x, y), c) ->
         let prev =
           CharMap.find_opt c mp |> Option.value ~default:mt_position_set
         in
         CharMap.add c (PositionSet.add { x; y } prev) mp)
       mt_c_to_pos
