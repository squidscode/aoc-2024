(* 
 *  Solves Advent of Code Problem 20:
 *  https://adventofcode.com/2024/day/20
 *)

module Positiontbl = Hashtbl.Make(Position.Iposition)
type ptod = int Positiontbl.t

let get_path_distances (pm : Position.pmap) : ptod =
  let start_position = pm
    |> Position.PositionMap.filter (fun _ c -> c = 'S')
    |> Position.PositionMap.to_list
    |> fun l -> assert (List.length l = 1) ; l
    |> List.hd
    |> fst
  in
  let end_position = pm
    |> Position.PositionMap.filter (fun _ c -> c = 'E')
    |> Position.PositionMap.to_list
    |> fun l -> assert (List.length l = 1) ; l
    |> List.hd
    |> fst
  in
  let pos_tbl = Positiontbl.create 1000 in
  let rec help (new_pos : Position.position) (i : int): unit =
    Positiontbl.add pos_tbl new_pos i;
    if new_pos = start_position then
      ()
    else
    Position.get_adjs new_pos
      |> List.filter (fun p ->
        Position.PositionMap.mem p pm && 
        (Position.PositionMap.find p pm = '.' ||
        Position.PositionMap.find p pm = 'S') &&
        not (Positiontbl.mem pos_tbl p)
      )
      |> List.iter (fun p -> help p (i+1));
    ()
  in
  help end_position 0;
  let ds = Positiontbl.find pos_tbl start_position in
  let de = Positiontbl.find pos_tbl end_position in
  Format.printf "Start: (%d, %d) = %d ; End: (%d, %d) = %d\n"
    start_position.x start_position.y ds end_position.x end_position.y de;
  pos_tbl

type cheat = (int * Position.position * Position.position)
type cheats = cheat list

let cheat_is_valid (pm : Position.pmap) ((_, p1, p2) : cheat) : bool =
  let diff = Position.pdiff p1 p2 in
  let mid : Position.position = match diff with
  | {x=2;y=0} -> {x=1;y=0}
  | {x=(-2);y=0} -> {x=(-1);y=0}
  | {x=0;y=2} -> {x=0;y=1}
  | {x=0;y=(-2)} -> {x=0;y=(-1)}
  | {x=1;y=1} -> {x=0;y=1}
  | {x=(-1);y=1} -> {x=0;y=1}
  | {x=(-1);y=(-1)} -> {x=0;y=(-1)}
  | {x=1;y=(-1)} -> {x=0;y=(-1)}
  | _ -> raise Not_found
  in
  let middle = Position.padd p1 mid in
  Position.PositionMap.find middle pm = '#'

let part_one (pm : Position.pmap) : int =
  let distances = get_path_distances pm in
  (*
  distances
    |> Positiontbl.iter (fun {x;y} dist -> 
      Format.printf "(%d,%d) at %d\n" x y dist
    );
  *)
  let process_cheats (pos : Position.position) : cheats =
    let a = Position.get_adjs pos in
    let ignore = a @ [pos] in
    let a' = a |> List.map Position.get_adjs |> List.flatten in
    let d = Positiontbl.find distances pos in
    a'
      |> List.filter (fun p -> not @@ List.mem p ignore)
      |> List.filter (fun p -> Positiontbl.mem distances p)
      |> List.map (fun p -> (Positiontbl.find distances p, p))
      |> List.map (fun (d', top) -> (d - d' - 2, pos, top))
  in
  distances
    |> Positiontbl.to_seq_keys
    |> Seq.map process_cheats
    |> List.of_seq
    |> List.flatten
    |> List.sort_uniq (fun (i1, pf1, pt1) (i2, pf2, pt2) -> 
        if i1 - i2 <> 0 then i1 - i2 else
        if Position.Iposition.compare pf1 pf2 <> 0 then 
          Position.Iposition.compare pf1 pf2 
        else Position.Iposition.compare pt1 pt2
    )
    |> List.map (fun ((i, { x=fx ; y=fy }, { x=tx ; y=ty }) as p : cheat) -> 
        Format.printf "(%d, (%d,%d) => (%d,%d))\n" i fx fy tx ty;
        p
      )
    |> List.filter (fun (i, _, _) -> 100 <= i)
    |> List.length

type cheat' = (int * int * Position.position * Position.position)
type cheats' = cheat' list
  
module Iadjmatch = struct
  type t = (int * Position.position)
  let hash ((i1, p) : t) : int =
    101 * i1 + Position.Iposition.hash p
  let equal (p1 : t) (p2 : t) : bool =
    p1 = p2
end
type adjmatch = Iadjmatch.t
module Adjmatchtbl = Hashtbl.Make(Iadjmatch)

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let part_two (pm : Position.pmap) : int =
  let distances = get_path_distances pm in
  let nskips = 20 in
  (* 
   * How do we use dynamic programming to make this algorithm go faster?
   * 
   * Suppose we know F(0, 6, (1,1)) maps to a list. Then if we want to find
   * F(0, 6, (1,2)) we can reuse computation from F(0, 6, (1,1)) because they are
   * adjacent!
   *
   * F(0, 6, (1,2)) will need 
   *   F(1, 5, (1,1)) | F(1, 5, (1,3)) | F(1, 5, (0,2)) | F(1, 5, (2,2))
   *    |> for duplicate position, use the one with the smallest step
   *
   * F(1,5,(1,1)) = F(0,6,(1,1)) |> step -> step+(1-0) |> filter step > 20
   * 
   * step , position --> (step, position) list
   *)
  let rec prune_duplicates (pls : adjmatch list) : adjmatch list =
    match pls with | [] -> [] | ((_, pos) as tup) :: _ -> 
    let withpos = pls |> List.filter (fun (_, p) -> p = pos) in
    let woutpos = pls |> List.filter (fun (_, p) -> p <> pos) in
    let bestpos = withpos 
    |> List.fold_left (fun ((d1, _) as tup1) ((d2, _) as tup2) ->
      if d1 < d2 then tup1 else tup2
    ) tup in
    bestpos :: (prune_duplicates woutpos)
  in

  let pc_tbl = Adjmatchtbl.create 1_000_000 in

  let rec _pc_hit (tbl : adjmatch list Adjmatchtbl.t) (((step, pos) as pair) : 
    (int * Position.position)) : adjmatch list option =
    if step = 0 || Adjmatchtbl.mem tbl pair then
      (* cache hit! *)
      Adjmatchtbl.find_opt tbl pair
    else 
      (* cache miss! *)
      _pc_hit tbl (step - 1, pos)
        |> fun op -> Option.bind op (fun l -> l 
          |> List.map (fun (stp, pos) -> (stp+1,pos))
          |> List.filter (fun (stp, _) -> stp <= nskips)
          |> Option.some
        )
  in

  let _process_cheats_v2 (pos : Position.position) : adjmatch list =
    (* y - nskips -> y + nskips *)
    List.init (2*nskips + 1) (fun i -> i - nskips)
      |> List.map (fun ydiff -> 
        let y = pos.y + ydiff in
        let yabs = Int.abs ydiff in
        let xlen = nskips - yabs in
        List.init (2*xlen + 1) (fun i -> i - xlen)
        |> List.map (fun xdiff -> 
          let x = pos.x + xdiff in
          let xabs = Int.abs xdiff in
          if Positiontbl.mem distances {x;y} then
            ((yabs + xabs, {x;y}) : adjmatch) |> Option.some
          else
            None
        )
        |> List.filter_map Fun.id
      )
      |> List.flatten
  in


  let rec _process_cheats ?(step : int = 0) (alen : int) (pos : Position.position)
    : adjmatch list =
    let cache_result = _pc_hit pc_tbl (step, pos) in
    if Option.is_some cache_result then
      Option.get cache_result
    else
    let cheats = [pos]
    |> List.filter (fun pos -> Positiontbl.mem distances pos)
    |> List.map (fun pos -> (step, pos)) in
    if alen = 0 then
      cheats
    else begin
      let rst = pos
        |> Position.get_adjs
        (* must be on the map *)
        |> List.filter (fun p -> Position.PositionMap.mem p pm) 
        |> List.map (_process_cheats ~step:(step + 1) (alen - 1))
        |> List.flatten
        |> prune_duplicates
      in
      let ans = rst @ cheats in
      Adjmatchtbl.add pc_tbl (step, pos) ans;
      ans
    end
  in

  let _n = Positiontbl.length distances in
  distances
    |> Positiontbl.to_seq_keys
    |> List.of_seq
    |> List.mapi (fun _i (p : Position.position) -> 
        Format.printf "%d / %d ; (%d,%d) cache size: %d\n" (_i+1) _n 
          p.x p.y (Adjmatchtbl.length pc_tbl);
        Format.print_flush ();
        _process_cheats_v2 p 
        |> List.map (fun (dist, pto) -> 
          (
            Positiontbl.find distances p - (Positiontbl.find distances pto + dist), 
            dist, p, pto
          )
        )
      )
    |> List.flatten
    |> List.sort_uniq (fun (i1, _, pf1, pt1) (i2, _, pf2, pt2) -> 
        if i1 - i2 <> 0 then i1 - i2 else
        if Position.Iposition.compare pf1 pf2 <> 0 then 
          Position.Iposition.compare pf1 pf2 
        else Position.Iposition.compare pt1 pt2
    )
    (*
    |> List.map (fun ((i, d, { x=fx ; y=fy }, { x=tx ; y=ty }) as p : cheat') -> 
        Format.printf "(%d, %d, (%d,%d) => (%d,%d))\n" i d fx fy tx ty;
        p
      )
    *)
    |> List.filter (fun (i, _, _, _) -> 100 <= i)
    |> List.length

let parse_file (file_content : string) : Position.pmap =
  file_content 
  |> String.split_on_char '\n' 
  |> List.filter ((<>) "")
  |> Position.get_pmap
