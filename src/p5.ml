(* 
 *  Solves Advent of Code Problem 5:
 *  https://adventofcode.com/2024/day/5
 *)

type rules = (int * int) list
type updates = int list list
type rules_and_updates = rules * updates

let adj_list (edges: (int * int) list): (int * int list) list =
  let rec add_rule_to_adj adj (l, r) =
    match adj with
    | (n, adjs) :: rst -> 
      if n = l then 
        (n, r :: adjs) :: rst
      else
        (n, adjs)      :: add_rule_to_adj rst (l, r)
    | []               -> [(l, [r])]
  in
  edges
    |> List.fold_left add_rule_to_adj []
;;

module IntMap = Map.Make(Int);;

let topological_sort (edges: (int * int) list): int list =
  let adjs = adj_list edges in
  let in_count = adjs
    |> List.fold_left (fun acc (i, l) -> (List.map (fun r -> (i, r)) l) @ acc) []
    |> List.map (fun (l, r) -> (r, l))
    |> adj_list
    |> List.map (fun (i, l) -> (i, List.length l))
    |> List.sort (fun (_, n1) (_, n2) -> n1 - n2)
    |> List.fold_left 
      (fun l i -> if Option.is_some (List.assoc_opt i l) then l else (i, 0) :: l)
    |> (fun f -> f (List.map (fun (l, _) -> l) edges))
    |> List.to_seq
    |> IntMap.of_seq
  in
  (*
  Format.printf "in count:\n";
  IntMap.iter (fun i n -> Format.printf "%d %d\n" i n) in_count;
  *)
  let rec help (acc: int list) (in_count: int IntMap.t): int list =
    let zeros: int list = in_count
      |> IntMap.filter (fun _ n -> n = 0)
      |> IntMap.to_seq
      |> Seq.map (fun (i, _) -> i) 
      |> List.of_seq in
    let non_zeros: int IntMap.t = in_count
      |> IntMap.filter (fun _ n -> n != 0) in
    (*
    Format.printf "zeros: "; List.iter (Format.printf "%d, ") zeros; Format.printf "\n";
    Format.printf "non_zeros: "; IntMap.iter (Format.printf "(%d, %d), ") non_zeros; Format.printf "\n";
    *)
    let new_in_count: int IntMap.t = zeros
      |> List.fold_left (fun mp i -> 
        List.assoc_opt i adjs
          |> Option.value ~default:[]
          |> List.fold_left (fun mp node -> IntMap.update node (Option.map (fun n -> n - 1)) mp) mp
      ) non_zeros
    in
    if List.length zeros = 0 then 
      acc 
    else
      help (acc @ zeros) new_in_count
  in
  help [] in_count
;;


let part_one (rules, updates: rules_and_updates): int =
  (*
  let adj = adj_list rules in
  Format.printf "Adj:";
  List.iter (fun (n, l) -> Format.printf "\n%d: " n; List.iter (Format.printf "%d, ") l) adj;
  Format.printf "\n";
  *)
  let rec is_increasing prev l =
    match l with
    | x :: xs -> if x >= prev then is_increasing x xs else false
    | []      -> true
  in
  let get_middle_or_zero update =
    let top = topological_sort (List.filter (
      fun (l, _) -> List.mem l update
    ) rules) in
    (*
    Format.printf "Topo: ";
    List.iter (Format.printf "%d, ") top;
    Format.printf "\n";
    *)
    let top_ind = top |> List.mapi (fun i node -> (node, i)) in
    let iupdate = update
     |> List.map (List.assoc_opt)
     |> List.map (fun f -> f top_ind)
     |> List.map (Option.value ~default:(-1)) in
    if is_increasing (List.hd iupdate) iupdate then 
      List.nth update (List.length update / 2)
    else
      0
  in
  updates |> List.map get_middle_or_zero |> List.fold_left (+) 0
;;

let part_two (rules, updates: rules_and_updates): int =
  let rec is_increasing prev l =
    match l with
    | x :: xs -> if x >= prev then is_increasing x xs else false
    | []      -> true
  in
  let get_middle_or_zero update =
    let top = topological_sort (List.filter (
      fun (l, _) -> List.mem l update
    ) rules) in
    let top_ind = top |> List.mapi (fun i node -> (node, i)) in
    let iupdate = update
     |> List.map (List.assoc_opt)
     |> List.map (fun f -> f top_ind)
     |> List.map (Option.value ~default:(-1)) in
    if is_increasing (List.hd iupdate) iupdate then 
      0
    else (* sort in topological and return middle *)
      update
        |> List.map (fun i -> (i, Option.value ~default:(-1) (List.assoc_opt i top_ind)))
        |> List.sort (fun (_, t1) (_, t2) -> t1 - t2)
        |> List.map (fun (n, _) -> n)
        |> List.nth
        |> fun f -> f (List.length update / 2)
  in
  updates |> List.map get_middle_or_zero |> List.fold_left (+) 0
;;

let parse_file (file_content: string): rules_and_updates =
  let lines = file_content
    |> String.split_on_char '\n' in
  let (empty_line_ind, _) = lines 
    |> List.mapi (fun i s -> (i,s)) 
    |> List.find (fun (_, s) -> s = "") in
  let rules = lines 
    |> List.to_seq 
    |> Seq.take empty_line_ind 
    |> List.of_seq
    |> List.map (String.split_on_char '|')
    |> List.map (fun l -> 
        (int_of_string (List.nth l 0), int_of_string (List.nth l 1))) in
  let updates = lines 
    |> List.to_seq 
    |> Seq.drop (empty_line_ind + 1) 
    |> List.of_seq 
    |> List.filter (fun s -> not @@ (String.equal "" s))
    |> List.map (String.split_on_char ',')
    |> List.map (List.map int_of_string)
  in
  (rules, updates)
;;
