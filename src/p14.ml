(*
 *  Solves Advent of Code Problem 14:
 *  https://adventofcode.com/2024/day/14
 *)

open Format

type position = { x : int; y : int }
type robot = { location : position; velocity : int * int }

let width = 101
let height = 103
let seconds = 8258

let print_locations (locations : position list) : unit =
  List.init height Fun.id
  |> List.iter (fun y ->
         List.init width Fun.id
         |> List.iter (fun x ->
                let num =
                  locations
                  |> List.filter (fun { x = px; y = py } -> x = px && y = py)
                  |> List.length
                in
                let s = if num = 0 then " " else string_of_int num in
                printf "%s" s);
         printf "\n")

let part_one (robots : robot list) : int =
  let robot_final_positions =
    robots
    |> List.map (fun { location = { x; y }; velocity = vx, vy } ->
           { x = x + (vx * seconds); y = y + (vy * seconds) })
    |> List.map (fun { x; y } ->
           let x = (x + (100000 * width)) mod width in
           let y = (y + (100000 * height)) mod height in
           { x; y })
  in
  print_locations robot_final_positions;
  let q1 =
    robot_final_positions
    |> List.filter (fun { x; y } -> x < width / 2 && y < height / 2)
  in
  let q2 =
    robot_final_positions
    |> List.filter (fun { x; y } -> x > width / 2 && y < height / 2)
  in
  let q3 =
    robot_final_positions
    |> List.filter (fun { x; y } -> x < width / 2 && y > height / 2)
  in
  let q4 =
    robot_final_positions
    |> List.filter (fun { x; y } -> x > width / 2 && y > height / 2)
  in
  [ q1; q2; q3; q4 ] |> List.map List.length
  |> List.fold_left (fun x y -> x * y) 1

let is_adj (p1 : position) (p2 : position) : bool =
  let x = p1.x - p2.x in
  let y = p1.y - p2.y in
  let dist = (x * x) + (y * y) in
  1 <= dist && dist <= 2

let pos_compare (p1 : position) (p2 : position) : int =
  let c1 = Int.compare p1.x p2.x in
  if c1 = 0 then c1 else Int.compare p1.y p2.y

let not_in (pl : position list) (p : position) : bool = not (List.mem p pl)
let print_position (p : position) : unit = printf "(%d, %d)" p.x p.y

let rec print_adjs (adjs : (position * position list) list) : unit =
  match adjs with
  | [] -> printf "\n"
  | (p, adj) :: rst ->
      print_position p;
      printf ": ";
      adj
      |> List.iter (fun p ->
             print_position p;
             printf ", ");
      printf "\n";
      print_adjs rst

let rec get_longest_path ?(seen : position list = []) (start : position)
    (adjs : (position * position list) list) : position list =
  let adj = List.assoc start adjs |> List.filter (not_in seen) in
  match adj with
  | [] -> [ start ]
  | _ ->
      let longest_paths =
        adj
        |> List.map (fun a ->
               start :: a :: get_longest_path ~seen:(start :: seen) a adjs)
        |> List.map (fun p -> (List.length p, p))
      in
      longest_paths
      |> List.fold_left
           (fun (blen, bp) (len, p) ->
             if blen < len then (len, p) else (blen, bp))
           (0, [])
      |> snd

let n_robots_are_close (n : int) (robots : position list) : bool =
  let robots = robots |> List.sort_uniq pos_compare in
  let robot_adj_map =
    robots |> List.map (fun r -> (r, robots |> List.filter (is_adj r)))
  in
  List.length @@ get_longest_path (List.hd robots) robot_adj_map >= n
(* The code below is too slow, it might be better just to take a
      random robot and measure the max path from that one
   robots
   |> List.fold_left (fun bool start ->
       bool || List.length (get_longest_path start robot_adj_map) >= n
     ) false
*)

let arent_overlapping (locations : position list) : bool =
  locations |> List.sort_uniq pos_compare |> List.length = List.length locations

let get_position (robot : robot) : position =
  match robot with { location; _ } -> location

let move_robot (robot : robot) : robot =
  match robot with
  | { location = { x; y }; velocity = vx, vy } ->
      {
        location =
          { x = (x + vx + width) mod width; y = (y + vy + height) mod height };
        velocity = (vx, vy);
      }

let rec pairs (locations : position list) : (position * position) list =
  match locations with
  | p :: rst -> (rst |> List.map (fun p2 -> (p, p2))) @ pairs rst
  | _ -> []

let dist (p1 : position) (p2 : position) : float =
  let x = p1.x - p2.x |> float_of_int in
  let y = p1.y - p2.y |> float_of_int in
  Float.sqrt ((x *. x) +. (y *. y))

let closeness (locations : position list) : float =
  let loc_pairs = pairs locations in
  let sum =
    loc_pairs
    |> List.map (fun (p1, p2) -> dist p1 p2)
    |> List.fold_left (fun a b -> a +. b) 0.
  in
  sum /. float_of_int (List.length loc_pairs)

let closeness_min = 30
let iterations = 9000

let part_two (robots : robot list) : int =
  printf "Num robots: %d\n" (List.length robots);
  List.init iterations Fun.id
  |> List.fold_left
       (fun robots i ->
         let new_robots = robots |> List.map move_robot in

         let robot_positions = new_robots |> List.map get_position in

         if i mod 1000 = 0 then printf "Iteration #%d done!\n" i;

         let closeness = closeness robot_positions in
         if closeness < 42. || n_robots_are_close closeness_min robot_positions
         then (
           (*
      if n_robots_are_close closeness_min robot_positions then begin
      if arent_overlapping robot_positions then begin
      *)
           printf "Iteration #%d:\n" (i + 1);
           printf "closeness: %.2f\n" closeness;
           print_locations robot_positions;
           printf "\n");
         flush_all ();
         new_robots)
       robots
  |> fun _ ->
  ();
  0

let re = "p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)\n"

let parse_file (file_content : string) : robot list =
  file_content
  |> Re.all (Re.Perl.compile (Re.Perl.re re))
  |> List.map (fun g ->
         let px = Re.Group.get g 1 in
         let py = Re.Group.get g 2 in
         let vx = Re.Group.get g 3 in
         let vy = Re.Group.get g 4 in
         {
           location = { x = int_of_string px; y = int_of_string py };
           velocity = (int_of_string vx, int_of_string vy);
         })
