(*
 *  Solves Advent of Code Problem 18:
 *  https://adventofcode.com/2024/day/18
 *)

open Position

module Iposdist = struct
  type t = position * int

  let compare ((p1, d1) : t) ((p2, d2) : t) : int =
    if d1 <> d2 then d1 - d2 else Iposition.compare p1 p2
end

module PositionDistanceSet = Set.Make (Iposdist)

type position_distance_set = PositionDistanceSet.t

let sz = 71

let within_bounds ({ x; y } : position) : bool =
  0 <= x && x < sz && 0 <= y && y < sz

let non_mem (walls : position_set) (p : position) : bool =
  PositionSet.mem p walls |> not

let non_mem' (seen : int PositionMap.t) (p : position) : bool =
  PositionMap.mem p seen |> not

let get_adjs ({ x; y } : position) : position list =
  [ { x = x + 1; y }; { x = x - 1; y }; { x; y = y + 1 }; { x; y = y - 1 } ]

let rec dijkstras ?(seen : int PositionMap.t = PositionMap.empty)
    (walls : position_set) (end_position : position)
    (hp : position_distance_set) : int option =
  (*
  Format.printf "hp sz: %d\n" (PositionDistanceSet.cardinal hp);
  hp |> PositionDistanceSet.iter (fun ({x;y}, d) ->
    Format.printf "(%d,%d) at %d ; " x y d
  );
  Format.printf "\n";
  *)
  if PositionDistanceSet.cardinal hp = 0 then
    None
  else
    let ((nxtp, nxtd) as p) = hp |> PositionDistanceSet.min_elt in
    let hp = PositionDistanceSet.remove p hp in

    if PositionMap.mem nxtp seen then
      dijkstras ~seen walls end_position hp
    else if nxtp = end_position then
      Some nxtd
    else
      let seen = PositionMap.add nxtp nxtd seen in

      let valid_adjs : Iposdist.t Seq.t =
        nxtp |> get_adjs |> List.filter within_bounds
        |> List.filter (non_mem walls)
        |> List.filter (non_mem' seen)
        |> List.map (fun p -> (p, nxtd + 1))
        |> List.to_seq
      in

      let hp = hp |> PositionDistanceSet.add_seq valid_adjs in

      dijkstras ~seen walls end_position hp

let nbytes = 1024

let part_one (coords : (int * int) list) : int =
  let corrupted =
    coords
    |> List.map (fun (x, y) : position -> { x; y })
    |> List.to_seq |> Seq.take nbytes |> PositionSet.of_seq
  in
  let hp = ({ x = 0; y = 0 }, 0) |> PositionDistanceSet.singleton in
  dijkstras corrupted { x = sz - 1; y = sz - 1 } hp |> Option.get

(* returns the first value of tk that causes dijkstras to return None *)
let rec solve (coords : (int * int) list) (tk : int) : int * int =
  assert (tk < List.length coords);
  Format.printf "%d / %d\n" tk (List.length coords);
  Format.print_flush ();
  let corrupted =
    coords
    |> List.map (fun (x, y) : position -> { x; y })
    |> List.to_seq |> Seq.take tk |> PositionSet.of_seq
  in
  let hp = ({ x = 0; y = 0 }, 0) |> PositionDistanceSet.singleton in
  match dijkstras corrupted { x = sz - 1; y = sz - 1 } hp with
  | None -> List.nth coords (tk - 1)
  | Some _ -> solve coords (tk + 1)

let part_two (coords : (int * int) list) : int =
  let x, y = solve coords nbytes in
  Format.printf "answer: (%d,%d)\n" x y;
  0

let re = "(\\d+),(\\d+)"

let parse_file (file_content : string) : (int * int) list =
  file_content
  |> Re.all (Re.Perl.compile (Re.Perl.re re))
  |> List.map (fun g ->
         (Re.Group.get g 1 |> int_of_string, Re.Group.get g 2 |> int_of_string))
