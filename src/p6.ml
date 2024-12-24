(*
 *  Solves Advent of Code Problem 6:
 *  https://adventofcode.com/2024/day/6
 *)

type coord = { x : int; y : int }
type direction = Up | Right | Down | Left
type guard = { position : coord; direction : direction }

type game = {
  width : int;
  height : int;
  guard : guard;
  obstructions : coord list;
}

let get_diff (direction : direction) : int * int =
  match direction with
  | Up -> (0, -1)
  | Right -> (1, 0)
  | Down -> (0, 1)
  | Left -> (-1, 0)

let apply_diff (coord : coord) ((dx, dy) : int * int) : coord =
  match coord with { x; y } -> { x = x + dx; y = y + dy }

let rotate_90_degrees_clockwise (direction : direction) : direction =
  match direction with Up -> Right | Right -> Down | Down -> Left | Left -> Up

let out_of_bounds (width : int) (height : int) ({ x; y } : coord) : bool =
  x <= -1 || y <= -1 || x >= width || y >= height

let set_add (x : 'a) (xs : 'a list) : 'a list =
  if List.mem x xs then xs else x :: xs

let set_add_pair (x : 'a * 'b) (xs : ('a * 'b) list) : ('a * 'b) list =
  if List.mem x xs then xs else x :: xs

let part_one (game : game) : int =
  match game with
  | { width; height; guard = { position; direction }; obstructions } ->
      let rec get_traveled_coords (acc : coord list) (gp : coord)
          (gd : direction) : coord list =
        let diff = get_diff gd in
        let new_coord = apply_diff gp diff in
        match
          (List.mem new_coord obstructions, out_of_bounds width height new_coord)
        with
        | _, true -> set_add gp acc
        | true, false ->
            get_traveled_coords acc gp (rotate_90_degrees_clockwise gd)
        | false, false -> get_traveled_coords (set_add gp acc) new_coord gd
      in
      get_traveled_coords [] position direction |> List.length

let part_two (game : game) : int =
  match game with
  | { width; height; guard = { position; direction }; obstructions } ->
      let rec get_traveled_coords (acc : (coord * direction) list) (gp : coord)
          (gd : direction) (obstructions : coord list) :
          (coord * direction) list option =
        let diff = get_diff gd in
        let new_coord = apply_diff gp diff in
        match
          ( List.mem (gp, gd) acc,
            List.mem new_coord obstructions,
            out_of_bounds width height new_coord )
        with
        | true, _, _ -> None
        | _, _, true -> Some (set_add_pair (gp, gd) acc)
        | _, true, false ->
            get_traveled_coords acc gp
              (rotate_90_degrees_clockwise gd)
              obstructions
        | _, false, false ->
            get_traveled_coords (set_add (gp, gd) acc) new_coord gd obstructions
      in
      let tc = get_traveled_coords [] position direction obstructions in
      let tc = Option.get tc in
      let tc =
        tc |> List.map fst
        |> List.filter (( <> ) position)
        |> List.fold_left (fun acc i -> set_add i acc) []
      in
      Format.printf "tc size: %d\n" (List.length tc);
      tc
      |> List.map List.cons (* pos *)
      |> List.map (( |> ) obstructions)
      |> List.mapi (fun i s ->
             if i mod 100 = 0 then
               Format.printf "\n#%d done" i
             else if i mod 10 = 0 then
               Format.printf ".";
             Format.print_flush ();
             get_traveled_coords [] position direction s)
      |> List.filter Option.is_none |> List.length

exception ParseError of string

let parse_file (file_content : string) : game =
  let lines : string list =
    file_content |> String.split_on_char '\n'
    |> List.filter (fun s -> not (String.equal "" s))
  in
  let coords : ((int * int) * char) list =
    lines |> List.map String.to_seqi
    |> List.mapi (fun y l ->
           Seq.map (fun (x, c) -> ((x, y), c)) l |> List.of_seq)
    |> List.flatten
  in
  let guard : guard =
    coords
    |> List.filter (fun (_, c) -> List.mem c [ '^'; '>'; 'v'; '<' ])
    |> List.hd
    |> fun ((x, y), c) ->
    {
      position = { x; y };
      direction =
        (match c with
        | '^' -> Up
        | '>' -> Left
        | 'v' -> Down
        | '<' -> Right
        | _ -> raise (ParseError "Bad guard direction"));
    }
  in
  let obstructions : coord list =
    coords
    |> List.filter (fun (_, c) -> c = '#')
    |> List.map (fun ((x, y), _) -> { x; y })
  in
  {
    width = String.length (List.hd lines);
    height = List.length lines;
    guard;
    obstructions;
  }
