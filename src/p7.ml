(* 
 *  Solves Advent of Code Problem 7:
 *  https://adventofcode.com/2024/day/7
 *)

type equation = { 
  target: int;
  values: int list;
};;

let can_be_evaluated (equation: equation): bool =
  let rec help (equation: equation): bool =
    match equation with { target ; values } ->
      match values with
      | x :: xs -> 
        help { target = target - x ; values = xs }
      || if target mod x = 0 then help { target = target / x ; values = xs } else false
      | []      -> target = 0
  in
  match equation with { target ; values } ->
    help { target = target ; values = List.rev values }
;;

let part_one (eqs: equation list): int =
  eqs
    |> List.map (fun ({target ; _} as eq) -> (can_be_evaluated eq, target))
    |> List.filter (fun (b, _) -> b)
    |> List.map (fun (_, t) -> t)
    |> List.fold_left (+) 0
;;

let can_be_evaluated_v2 (equation: equation): bool =
  let is_inv_concatable target value: bool =
    let digits = floor ((log10 (float_of_int value)) +. 1.0) |> int_of_float in
    let m = target - value in
    m mod (int_of_float (Float.pow 10. (float_of_int digits))) = 0
  in
  let inv_concat target value: int =
    let digits = floor ((log10 (float_of_int value)) +. 1.0) |> int_of_float in
    let m = target - value in
    m / (int_of_float (Float.pow 10. (float_of_int digits)))
  in
  (*
  let rec reverse_int ?(acc: int=0) (x: int): int =
    match x with
    | 0 -> acc
    | _ -> reverse_int ~acc:(10 * acc + (x mod 10)) (x / 10)
  in
  Format.printf "%d\n" (reverse_int 1024);
  *)
  let rec help ?(concat_acc: int=0) (equation: equation): bool =
    match equation with { target ; values } ->
      Format.printf "target = %d ; concat_acc = %d ; values = " target concat_acc;
      List.iter (Format.printf "%d ") values;
      Format.printf "\n";
      match values with
      | x :: xs -> 
        help { target = target - x ; values = xs }
      || (if target mod x = 0 then 
        help { target = target / x ; values = xs } else false)
      || (if is_inv_concatable target x then 
        help { target = (inv_concat target x) ; values = xs } else false
      )
      (*
      || (if is_inv_concatable target (concat_acc + x) then
        help { target = (inv_concat target (concat_acc + x)) ; values = xs } else false)
      || (if is_inv_concatable target (concat_acc * x) then
        help { target = (inv_concat target (concat_acc * x)) ; values = xs } else false)
      || help ~concat_acc:(concat_acc + x) { target = target ; values = xs }
      || help ~concat_acc:(concat_acc * x) { target = target ; values = xs }
      *)
      | []      -> target = 0 || concat_acc = target
  in
  match equation with { target ; values } ->
    help { target = target ; values = List.rev values }
;;

let part_two (eqs: equation list): int =
  eqs
    |> List.map (fun ({target ; _} as eq) -> (can_be_evaluated_v2 eq, target))
    |> List.filter (fun (b, _) -> b)
    |> List.map (fun (_, t) -> t)
    |> List.fold_left (+) 0
;;

let parse_file (file_content: string): equation list =
  file_content
    |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length s <> 0)
    |> List.map (Re.all (Re.Perl.compile (Re.Perl.re "(\\d+): (.*)")))
    |> List.map (fun gl -> assert (List.length gl = 1); List.hd gl)
    |> List.map (fun g -> (Re.Group.get g 1, Re.Group.get g 2))
    |> List.map (fun (t, vs) -> (int_of_string t, String.split_on_char ' ' vs))
    |> List.map (fun (t, vs) -> (t, List.filter (fun s -> String.length s <> 0) vs))
    |> List.map (fun (t, vs) -> {
      target = t ; values = vs |> List.map int_of_string
    })
;;
