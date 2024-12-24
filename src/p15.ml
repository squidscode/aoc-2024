(*
 *  Solves Advent of Code Problem 15:
 *  https://adventofcode.com/2024/day/15
 *)

open Position

exception ParseException

type move = Up | Down | Left | Right
type instruction = move list
type puzzle = { grid : grid; instructions : instruction list }

let move_to_char move =
  match move with Up -> '^' | Down -> 'v' | Left -> '<' | Right -> '>'

let get_next (p : position) (move : move) : position =
  match move with
  | Up -> { x = p.x; y = p.y - 1 }
  | Down -> { x = p.x; y = p.y + 1 }
  | Left -> { x = p.x - 1; y = p.y }
  | Right -> { x = p.x + 1; y = p.y }

(* Push the box at location pos in the direction of move,
   and return the new box locations if possible.
   NOTE: pos should be vacant from boxes after a move_boxes call *)
let rec move_box (pos : position) (move : move) (boxes : position_set)
    (walls : position_set) : position_set option =
  assert (PositionSet.mem pos boxes);
  let boxes_without_pos = PositionSet.filter (fun p -> p <> pos) boxes in
  let nxt_pos = get_next pos move in
  let nxt_pos_in_wall = PositionSet.mem nxt_pos walls in
  let nxt_pos_in_box = PositionSet.mem nxt_pos boxes in
  match (nxt_pos_in_wall, nxt_pos_in_box) with
  | true, _ -> None (* if the next position is a wall, we can't push! *)
  | false, false -> PositionSet.add nxt_pos boxes_without_pos |> Option.some
  | false, true ->
      (* propagate push box *)
      let attempt_push = move_box nxt_pos move boxes_without_pos walls in
      Option.bind attempt_push (fun push_boxes ->
          PositionSet.add nxt_pos push_boxes |> Option.some)

let replace_robot (pos : position) (grid : grid) : grid =
  let pset = PositionSet.add pos empty_position_set in
  CharMap.add '@' pset grid

let take_move (move : move) (grid : grid) : grid option =
  let robot =
    grid |> grid_get_pset '@' |> PositionSet.find_first (fun _ -> true)
  in
  let boxes = grid |> grid_get_pset 'O' in
  let walls = grid |> grid_get_pset '#' in
  let nxt_robot_position = get_next robot move in
  let pos_in_wall = PositionSet.mem nxt_robot_position walls in
  let pos_in_boxes = PositionSet.mem nxt_robot_position boxes in
  match (pos_in_wall, pos_in_boxes) with
  | true, _ -> None
  | false, false -> Some (replace_robot nxt_robot_position grid)
  | false, true ->
      let new_boxes = move_box nxt_robot_position move boxes walls in
      Option.bind new_boxes (fun new_boxes ->
          grid |> CharMap.add 'O' new_boxes
          |> replace_robot nxt_robot_position
          |> Option.some)

let add_coord ({ x; y } : position) (sum : int) : int = (100 * y) + x + sum

let rec disjoint (positions : position_set list) : bool =
  match positions with
  | ps :: rst ->
      rst
      |> List.fold_left
           (fun b pset -> b && PositionSet.disjoint ps pset)
           (disjoint rst)
  | [] -> true

let part_one ({ grid; instructions } : puzzle) : int =
  let instructions = instructions |> List.flatten in
  Format.printf "INITIAL GRID: \n";
  Format.printf "Num instructions: %d\n" (List.length instructions);
  print_grid grid;
  Format.printf "\n";
  instructions
  |> List.mapi (fun i move -> (i, move))
  |> List.fold_left
       (fun grid (i, move) ->
         (* attempt to take a move *)
         let new_grid = take_move move grid |> Option.value ~default:grid in
         assert (disjoint (CharMap.to_seq new_grid |> Seq.map snd |> List.of_seq));
         assert (CharMap.find '@' new_grid |> PositionSet.cardinal = 1);
         Format.printf "Move #%d: %c\n" i (move_to_char move);
         (* print_grid new_grid;*)
         Format.printf "\n";
         new_grid)
       grid
  |> CharMap.find 'O'
  |> fun ps -> PositionSet.fold add_coord ps 0

let doublex ({ x; y } : position) : position = { x = 2 * x; y }

let double (grid : grid) : grid =
  let robot =
    grid |> grid_get_pset '@' |> PositionSet.find_first (fun _ -> true)
  in
  let boxes = grid |> grid_get_pset 'O' in
  let walls = grid |> grid_get_pset '#' in
  let new_robot : position_set =
    doublex robot |> fun p -> PositionSet.add p empty_position_set
  in
  let new_lboxes = boxes |> PositionSet.map doublex in
  let new_rboxes =
    new_lboxes
    |> PositionSet.map (fun ({ x; y } : position) -> { x = x + 1; y })
  in
  let new_walls =
    walls |> PositionSet.map doublex |> PositionSet.to_seq
    |> Seq.map (fun (p : position) ->
           match p with { x; y } -> [ p; { x = x + 1; y } ])
    |> List.of_seq |> List.flatten |> PositionSet.of_list
  in
  empty_grid |> CharMap.add '@' new_robot |> CharMap.add '[' new_lboxes
  |> CharMap.add ']' new_rboxes |> CharMap.add '#' new_walls

let move_is_vertical move = match move with Up | Down -> true | _ -> false
let move_is_horizontal move = not (move_is_vertical move)

(* Push the box at location pos in the direction of move,
   and return the new box locations if possible.
   NOTE: pos should be vacant from boxes after a move_boxes call *)
let rec move_box_horizontal (pos : position) (move : move)
    (lboxes : position_set) (rboxes : position_set) (walls : position_set) :
    (position_set * position_set) option =
  assert (PositionSet.mem pos lboxes || PositionSet.mem pos rboxes);
  assert (move_is_horizontal move);

  let lboxes_without_pos = PositionSet.filter (fun p -> p <> pos) lboxes in
  let rboxes_without_pos = PositionSet.filter (fun p -> p <> pos) rboxes in
  let nxt_pos = get_next pos move in
  let nxt_pos_in_wall = PositionSet.mem nxt_pos walls in
  let nxt_pos_in_lbox = PositionSet.mem nxt_pos lboxes in
  let nxt_pos_in_rbox = PositionSet.mem nxt_pos rboxes in
  match (nxt_pos_in_wall, nxt_pos_in_lbox, nxt_pos_in_rbox) with
  | true, _, _ -> None (* if the next position is a wall, we can't push! *)
  (* The next box is free! *)
  | false, false, false when PositionSet.mem pos lboxes ->
      (PositionSet.add nxt_pos lboxes_without_pos, rboxes_without_pos)
      |> Option.some
  | false, false, false when PositionSet.mem pos rboxes ->
      (lboxes_without_pos, PositionSet.add nxt_pos rboxes_without_pos)
      |> Option.some
      (* Attempting to push into another box *)
  | false, true, false when PositionSet.mem pos rboxes ->
      (* propagate push box *)
      (* Pushing an rbox and a lbox exists in the next position *)
      let attempt_push =
        move_box_horizontal nxt_pos move lboxes_without_pos rboxes_without_pos
          walls
      in
      Option.bind attempt_push (fun (lboxes, rboxes) ->
          (lboxes, PositionSet.add nxt_pos rboxes) |> Option.some)
  | false, false, true when PositionSet.mem pos lboxes ->
      (* propagate push box *)
      (* Pushing an lbox and a rbox exists in the next position *)
      let attempt_push =
        move_box_horizontal nxt_pos move lboxes_without_pos rboxes_without_pos
          walls
      in
      Option.bind attempt_push (fun (lboxes, rboxes) ->
          (PositionSet.add nxt_pos lboxes, rboxes) |> Option.some)
  | _ -> raise Not_found

(* make sure that pos is a lbox *)
let rec move_box_vertical (lpos : position) (move : move)
    (lboxes : position_set) (rboxes : position_set) (walls : position_set) :
    (position_set * position_set) option =
  let rpos : position = { x = lpos.x + 1; y = lpos.y } in

  assert (PositionSet.mem lpos lboxes && PositionSet.mem rpos rboxes);
  assert (move_is_vertical move);

  let lboxes_without_pos = PositionSet.filter (fun p -> p <> lpos) lboxes in
  let rboxes_without_pos = PositionSet.filter (fun p -> p <> rpos) rboxes in
  let nxt_lpos = get_next lpos move in
  let nxt_rpos = get_next rpos move in
  let nxt_pos_in_wall =
    PositionSet.mem nxt_lpos walls || PositionSet.mem nxt_rpos walls
  in
  (* If we attempt to move into an obstacle, then we return None *)
  if nxt_pos_in_wall then
    None
  else if
    (* boxes are stacked without splitting *)
    PositionSet.mem nxt_lpos lboxes_without_pos
  then
    let move_attempt =
      move_box_vertical nxt_lpos move lboxes_without_pos rboxes_without_pos
        walls
    in
    Option.bind move_attempt (fun (lboxes, rboxes) ->
        (PositionSet.add nxt_lpos lboxes, PositionSet.add nxt_rpos rboxes)
        |> Option.some)
  else if
    (* left and right split *)
    PositionSet.mem nxt_lpos rboxes_without_pos
    && PositionSet.mem nxt_rpos lboxes_without_pos
  then
    let llpos : position = { x = nxt_lpos.x - 1; y = nxt_lpos.y } in
    let lmove_attempt =
      move_box_vertical llpos move lboxes_without_pos rboxes_without_pos walls
    in
    Option.bind lmove_attempt (fun (lboxes, rboxes) ->
        let rmove_attempt =
          move_box_vertical nxt_rpos move lboxes rboxes walls
        in
        Option.bind rmove_attempt (fun (lboxes, rboxes) ->
            (PositionSet.add nxt_lpos lboxes, PositionSet.add nxt_rpos rboxes)
            |> Option.some))
  else if (* left split *)
          PositionSet.mem nxt_lpos rboxes_without_pos then
    let llpos : position = { x = nxt_lpos.x - 1; y = nxt_lpos.y } in
    let lmove_attempt =
      move_box_vertical llpos move lboxes_without_pos rboxes_without_pos walls
    in
    Option.bind lmove_attempt (fun (lboxes, rboxes) ->
        (PositionSet.add nxt_lpos lboxes, PositionSet.add nxt_rpos rboxes)
        |> Option.some)
  else if (* right split *)
          PositionSet.mem nxt_rpos lboxes_without_pos then
    let rmove_attempt =
      move_box_vertical nxt_rpos move lboxes_without_pos rboxes_without_pos
        walls
    in
    Option.bind rmove_attempt (fun (lboxes, rboxes) ->
        (PositionSet.add nxt_lpos lboxes, PositionSet.add nxt_rpos rboxes)
        |> Option.some)
  else (* l and r are both open *)
    let box_union = PositionSet.union rboxes_without_pos lboxes_without_pos in
    if
      (not (PositionSet.mem nxt_lpos box_union))
      && not (PositionSet.mem nxt_rpos box_union)
    then
      ( PositionSet.add nxt_lpos lboxes_without_pos,
        PositionSet.add nxt_rpos rboxes_without_pos )
      |> Option.some
    else
      raise Not_found

let get_move_box_fn (move : move) =
  if move_is_horizontal move then move_box_horizontal else move_box_vertical

let take_move_p2 (move : move) (grid : grid) : grid option =
  let robot =
    grid |> grid_get_pset '@' |> PositionSet.find_first (fun _ -> true)
  in
  let lboxes = grid |> grid_get_pset '[' in
  let rboxes = grid |> grid_get_pset ']' in
  let walls = grid |> grid_get_pset '#' in
  let nxt_robot_position = get_next robot move in
  let pos_in_wall = PositionSet.mem nxt_robot_position walls in
  let pos_in_lboxes = PositionSet.mem nxt_robot_position lboxes in
  let pos_in_rboxes = PositionSet.mem nxt_robot_position rboxes in
  let move_box = get_move_box_fn move in
  match (pos_in_wall, pos_in_lboxes, pos_in_rboxes) with
  | true, _, _ -> None
  | false, false, false -> Some (replace_robot nxt_robot_position grid)
  | false, true, false | false, false, true ->
      let lbox_position : position =
        if pos_in_lboxes || move_is_horizontal move then
          nxt_robot_position
        else
          { x = nxt_robot_position.x - 1; y = nxt_robot_position.y }
      in
      let new_boxes = move_box lbox_position move lboxes rboxes walls in
      Option.bind new_boxes (fun (lboxes, rboxes) ->
          grid |> CharMap.add '[' lboxes |> CharMap.add ']' rboxes
          |> replace_robot nxt_robot_position
          |> Option.some)
  | _ -> raise Not_found

let part_two ({ grid; instructions } : puzzle) : int =
  let instructions = instructions |> List.flatten in
  let grid = double grid in
  Format.printf "INITIAL GRID: \n";
  Format.printf "Num instructions: %d\n" (List.length instructions);
  print_grid grid;
  Format.printf "\n";
  instructions
  |> List.mapi (fun i move -> (i, move))
  |> List.fold_left
       (fun grid (i, move) ->
         (* attempt to take a move *)
         Format.printf "Move #%d: %c\n" i (move_to_char move);
         let new_grid = take_move_p2 move grid |> Option.value ~default:grid in
         assert (disjoint (CharMap.to_seq new_grid |> Seq.map snd |> List.of_seq));
         assert (CharMap.find '@' new_grid |> PositionSet.cardinal = 1);
         (*print_grid new_grid;*)
         Format.printf "\n";
         new_grid)
       grid
  |> CharMap.find '['
  |> fun ps -> PositionSet.fold add_coord ps 0

let to_instruction (s : string) : instruction =
  s |> String.to_seq
  |> Seq.map (fun c ->
         match c with
         | '^' -> Up
         | '>' -> Right
         | 'v' -> Down
         | '<' -> Left
         | _ -> raise ParseException)
  |> List.of_seq

let parse_file (file_content : string) : puzzle =
  let lines = file_content |> String.split_on_char '\n' in
  let nl =
    lines
    |> List.mapi (fun i s -> (i, s))
    |> List.find (fun (_, s) -> s = "")
    |> fst
  in
  let grid =
    lines |> List.to_seq |> Seq.take nl |> List.of_seq |> get_grid
    |> CharMap.remove '.'
  in
  let instructions =
    lines |> List.to_seq
    |> Seq.drop (nl + 1)
    |> List.of_seq |> List.map to_instruction
  in
  assert (disjoint (CharMap.to_seq grid |> Seq.map snd |> List.of_seq));
  { grid; instructions }
(* split by whitespace
    |> List.map (Str.split (Str.regexp "[ \t]+"))
    |> List.map (List.filter (fun s -> not @@ String.equal "" s))
*)
