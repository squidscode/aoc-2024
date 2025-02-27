(** A library for creating and manipulating grids.
  *
  * Use `get_grid` to create a `grid`, which maps characters to a set of
  * positions. Any functions that manipulate positions should be added to this
  * file.
  *
  **)

open Format

module Iposition = struct
  type t = { x : int; y : int }

  let compare { x = x1; y = y1 } { x = x2; y = y2 } : int =
    let xc = Int.compare x1 x2 in
    if xc != 0 then xc else Int.compare y1 y2

  let equal (p1 : t) (p2 : t) : bool =
    p1 = p2

  let hash ({x;y} : t) : int =
    101 * x + y
end

type position = Iposition.t

module PositionSet = Set.Make (Iposition)

type position_set = PositionSet.t

module CharMap = Map.Make (Char)

type grid = position_set CharMap.t

let empty_grid : grid = CharMap.empty
let empty_position_set : position_set = PositionSet.empty

module PositionMap = Map.Make (Iposition)

type pmap = char PositionMap.t

let empty_pmap : pmap = PositionMap.empty

let pdiff (p1 : position) (p2 : position) : position =
  { x = p1.x - p2.x; y = p1.y - p2.y }

let padd (p1 : position) (p2 : position) : position =
  { x = p1.x + p2.x; y = p1.y + p2.y }

let padj (p1 : position) (p2 : position) : bool =
  let d = pdiff p1 p2 in
  (d.x * d.x) + (d.y * d.y) = 1

let get_adjs (p : position) : position list =
  [
    {x=p.x + 1 ; y=p.y};
    {x=p.x - 1 ; y=p.y};
    {x=p.x ; y=p.y + 1};
    {x=p.x ; y=p.y - 1}
  ]

let psprint (pset : position_set) : unit =
  pset |> PositionSet.iter (fun p -> printf "(%d, %d); " p.x p.y)

let grid_get_pset (c : char) (grid : grid) : position_set =
  grid |> CharMap.find_opt c |> Option.value ~default:empty_position_set

let print_grid_wh ?(default = '.') (width : int) (height : int) (grid : grid) :
    unit =
  let l =
    grid |> CharMap.to_seq |> List.of_seq
    |> List.map (fun (c, p) ->
           List.map (fun p -> (p, c)) (PositionSet.to_seq p |> List.of_seq))
    |> List.flatten
  in
  List.init height Fun.id
  |> List.iter (fun y ->
         List.init width Fun.id
         |> List.iter (fun x ->
                let p : position = { x; y } in
                Format.printf "%c"
                  (l |> List.assoc_opt p |> Option.value ~default));
         Format.printf "\n")

let print_grid ?(default = '.') (grid : grid) : unit =
  let pos =
    grid |> fun grid ->
    CharMap.fold
      (fun _ ps1 ps2 -> PositionSet.union ps1 ps2)
      grid empty_position_set
  in
  let p = pos |> PositionSet.max_elt in
  print_grid_wh ~default (p.x + 1) (p.y + 1) grid

let get_grid (lines : string list) : grid =
  lines
  |> List.filter (fun s -> String.length s <> 0)
  |> List.map String.to_seqi |> List.map List.of_seq
  |> List.mapi (fun y l -> List.map (fun (x, c) -> ((x, y), c)) l)
  |> List.flatten
  |> List.fold_left
       (fun mp ((x, y), c) ->
         let prev =
           CharMap.find_opt c mp |> Option.value ~default:empty_position_set
         in
         CharMap.add c (PositionSet.add { x; y } prev) mp)
       empty_grid

let pmap_get_positions (c : char) (pm : pmap) : position_set =
  pm |> PositionMap.to_seq
  |> Seq.filter (fun (_, c') -> c = c')
  |> Seq.map (fun (p, _) -> p)
  |> PositionSet.of_seq

let pmap_remove (c : char) (pm : pmap) : pmap =
  pm |> PositionMap.to_seq
  |> Seq.filter (fun (_, c') -> c <> c')
  |> PositionMap.of_seq

let print_pmap_wh ?(default = '.') (width : int) (height : int) (pmap : pmap) :
    unit =
  List.init height Fun.id
  |> List.iter (fun y ->
         List.init width Fun.id
         |> List.iter (fun x ->
                let p : position = { x; y } in
                Format.printf "%c"
                  (pmap |> PositionMap.find_opt p |> Option.value ~default));
         Format.printf "\n")


let find_position (mp : pmap) (c : char) : position option =
  mp
  |> PositionMap.to_seq
  |> Seq.filter (fun (_, c') -> c' = c)
  |> Seq.uncons
  |> Fun.flip Option.bind (fun ((p, _), _) -> p |> Option.some)

let print_pmap ?(default = '.') (pmap : pmap) : unit =
  let all_positions : position Seq.t =
    pmap |> PositionMap.to_seq |> Seq.map (fun (p, _) -> p)
  in
  let width =
    all_positions
    |> Seq.map (fun ({ x; _ } : position) -> x)
    |> Seq.fold_left max 0 |> ( + ) 1
  in
  let height =
    all_positions
    |> Seq.map (fun ({ y; _ } : position) -> y)
    |> Seq.fold_left max 0 |> ( + ) 1
  in
  print_pmap_wh ~default width height pmap

module Iposdist = struct
  type t = position * int
  let compare ((p1, d1) : t) ((p2, d2) : t) : int =
    if d1 <> d2 then d1 - d2 else Iposition.compare p1 p2
end
module PositionDistanceSet = Set.Make (Iposdist)
type position_distance_set = PositionDistanceSet.t

let rec dijkstras ?(seen : int PositionMap.t = PositionMap.empty) 
  (width : int) (height : int) (obstacles : position_set) (hp : position_distance_set) 
  : int PositionMap.t =
  if PositionDistanceSet.cardinal hp = 0 then
    seen
  else
    let ((nxtp, nxtd) as p) = hp |> PositionDistanceSet.min_elt in
    let hp = PositionDistanceSet.remove p hp in
    if PositionMap.mem nxtp seen then
      dijkstras ~seen width height obstacles hp
    else
      let seen = PositionMap.add nxtp nxtd seen in
      let valid_adjs : Iposdist.t Seq.t = nxtp 
        |> get_adjs 
        |> List.filter (fun ({x;y} : position) ->  0<=x && x<width && 0<=y && y<height)
        |> List.filter (Fun.negate @@ Fun.flip PositionSet.mem obstacles)
        |> List.filter (Fun.negate @@ Fun.flip PositionMap.mem seen)
        |> List.map (fun p -> (p, nxtd + 1))
        |> List.to_seq
      in
      let hp = hp |> PositionDistanceSet.add_seq valid_adjs in
      dijkstras ~seen width height obstacles hp


let get_pmap (lines : string list) : pmap =
  lines
  |> List.filter (fun s -> String.length s <> 0)
  |> List.map String.to_seqi |> List.map List.of_seq
  |> List.mapi (fun y l -> List.map (fun (x, c) -> ((x, y), c)) l)
  |> List.flatten
  |> List.fold_left
       (fun mp ((x, y), c) -> PositionMap.add { x; y } c mp)
       empty_pmap
