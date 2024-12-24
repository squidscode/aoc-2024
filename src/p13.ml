(*
 *  Solves Advent of Code Problem 13:
 *  https://adventofcode.com/2024/day/13
 *)

open Format

type position = { x : int; y : int }
type configuration = { a : position; b : position; prize : position }
type mat2d = { a : float; b : float; c : float; d : float }

let inverse (mat : mat2d) : mat2d =
  let d = 1. /. ((mat.a *. mat.d) -. (mat.b *. mat.c)) in
  {
    a = d *. mat.d;
    b = 0. -. (d *. mat.b);
    c = 0. -. (d *. mat.c);
    d = d *. mat.a;
  }

let mult (mat : mat2d) ((v1, v2) : float * float) : float * float =
  ((mat.a *. v1) +. (mat.b *. v2), (mat.c *. v1) +. (mat.d *. v2))

let print_configuration (config : configuration) : unit =
  printf "A: X+%d, Y+%d ; B: X+%d, Y+%d ; Prize: X=%d, Y=%d" config.a.x
    config.a.y config.b.x config.b.y config.prize.x config.prize.y

let delta = 0.001

let part_one (configs : configuration list) : int =
  configs
  |> List.iter (fun c ->
         print_configuration c;
         printf "\n");
  configs
  |> List.map (fun { a; b; prize = { x; y } } ->
         ( {
             a = float_of_int a.x;
             b = float_of_int b.x;
             c = float_of_int a.y;
             d = float_of_int b.y;
           },
           (float_of_int x, float_of_int y) ))
  |> List.map (fun (mat, v) -> mult (inverse mat) v)
  |> List.filter (fun (x, y) ->
         printf "(%f, %f)\n" x y;
         let xf = Float.round x in
         let yf = Float.round y in
         xf -. delta < x
         && xf +. delta > x
         && yf -. delta < y
         && yf +. delta > y)
  |> List.map (fun (x, y) ->
         (int_of_float (Float.round x), int_of_float (Float.round y)))
  |> List.map (fun (x, y) ->
         let ans = (3 * x) + y in
         printf "3*%d + %d = %d\n" x y ans;
         ans)
  |> List.fold_left ( + ) 0

let bias = 10000000000000.

let part_two (configs : configuration list) : int =
  configs
  |> List.iter (fun c ->
         print_configuration c;
         printf "\n");
  configs
  |> List.map (fun { a; b; prize = { x; y } } ->
         ( {
             a = float_of_int a.x;
             b = float_of_int b.x;
             c = float_of_int a.y;
             d = float_of_int b.y;
           },
           (bias +. float_of_int x, bias +. float_of_int y) ))
  |> List.map (fun (mat, v) -> mult (inverse mat) v)
  |> List.filter (fun (x, y) ->
         printf "(%f, %f)\n" x y;
         let xf = Float.round x in
         let yf = Float.round y in
         xf -. delta < x
         && xf +. delta > x
         && yf -. delta < y
         && yf +. delta > y)
  |> List.map (fun (x, y) ->
         (int_of_float (Float.round x), int_of_float (Float.round y)))
  |> List.map (fun (x, y) ->
         let ans = (3 * x) + y in
         printf "3*%d + %d = %d\n" x y ans;
         ans)
  |> List.fold_left ( + ) 0

let block_str =
  "Button A: X\\+(\\d+), Y\\+(\\d+)\n\
   Button B: X\\+(\\d+), Y\\+(\\d+)\n\
   Prize: X=(\\d+), Y=(\\d+)\n"

let parse_file (file_content : string) : configuration list =
  file_content
  |> Re.all (Re.Perl.compile (Re.Perl.re block_str))
  |> List.map (fun g ->
         {
           a =
             {
               x = int_of_string (Re.Group.get g 1);
               y = int_of_string (Re.Group.get g 2);
             };
           b =
             {
               x = int_of_string (Re.Group.get g 3);
               y = int_of_string (Re.Group.get g 4);
             };
           prize =
             {
               x = int_of_string (Re.Group.get g 5);
               y = int_of_string (Re.Group.get g 6);
             };
         })
