(*
 *  Solves Advent of Code Problem 9:
 *  https://adventofcode.com/2024/day/9
 *)

exception NotReachable
exception NotImplemented

type block = File of int | Free
type disk = block list

let c_to_int (c : char) = int_of_char c - int_of_char '0'

let rec expand ?(id : int = 0) (encoded : char list) : block list =
  match encoded with
  | fl :: fs :: rst ->
      List.init (c_to_int fl) (fun _ -> File id)
      @ List.init (c_to_int fs) (fun _ -> Free)
      @ expand ~id:(id + 1) rst
  | [ fl ] -> List.init (c_to_int fl) (fun _ -> File id)
  | [] -> []

let is_free (block : block) : bool =
  match block with Free -> true | _ -> false

let print_blocks (blocks : block list) =
  blocks
  |> List.map (fun b ->
         match b with Free -> "." | File id -> Format.sprintf "%d" id)
  |> List.iter (fun s -> Format.printf "%s" s);
  Format.printf "\n"

let is_all_free (blocks : block list) : bool =
  blocks |> List.fold_left (fun flag b -> flag && is_free b) true

let rec get_last_id ?(acc : int = -1) (blocks : block list) : int =
  match blocks with
  | Free :: rst -> get_last_id ~acc rst
  | File id :: rst -> get_last_id ~acc:id rst
  | [] -> acc

let rec remove_last_file (blocks : block list) : block list =
  match blocks with
  | Free :: rst -> Free :: remove_last_file rst
  | file :: rst ->
      if is_all_free rst then Free :: rst else file :: remove_last_file rst
  | [] -> []

let rec reorder ?(acc = []) (blocks : block list) : block list =
  (* print_blocks blocks; *)
  match blocks with
  | Free :: rst ->
      let is_all_free = is_all_free blocks in
      if is_all_free then
        List.rev acc @ [ Free ] @ blocks
      else
        let lst_id = get_last_id rst in
        let rmv_lst = remove_last_file rst in
        reorder ~acc:(File lst_id :: acc) rmv_lst
  | f :: rst -> reorder ~acc:(f :: acc) rst
  | [] -> List.rev acc

let part_one_slow (dm : string) : int =
  let expanded = dm |> String.to_seq |> List.of_seq |> expand in
  let reordered = reorder expanded in
  (* print_blocks reordered; *)
  reordered
  |> List.mapi (fun i b -> (i, b))
  |> List.fold_left
       (fun acc (i, b) -> match b with File id -> acc + (i * id) | _ -> acc)
       0

let part_one (dm : string) : int =
  let expanded =
    dm |> String.to_seq |> List.of_seq |> expand
    |> List.mapi (fun i b -> (i, b))
  in
  let reversed =
    expanded |> List.rev |> List.filter (fun (_, b) -> Fun.negate is_free b)
  in
  (* print_blocks reordered; *)
  expanded
  |> List.fold_left
       (fun (acc, rev) (i, b) ->
         match b with
         | File id as f ->
             if List.mem (i, f) rev then (acc + (i * id), rev) else (acc, rev)
         | Free -> (
             match rev with
             | (ri, File rid) :: rrst ->
                 if ri > i then (acc + (i * rid), rrst) else (acc, rev)
             | [] -> (acc, rev)
             | _ -> raise NotReachable))
       (0, reversed)
  |> fst

type rle = (int * block) list

let expand_into_rle (cs : char list) : rle =
  let rec help ?(pv : (int * block) option = None) (blocks : block list) : rle =
    match (pv, blocks) with
    | None, x :: xs -> help ~pv:(Some (1, x)) xs
    | None, [] -> []
    | Some (f, px), x :: xs ->
        if px = x then
          help ~pv:(Some (f + 1, px)) xs
        else
          (f, px) :: help ~pv:(Some (1, x)) xs
    | Some (f, px), [] -> [ (f, px) ]
  in
  let expanded = expand cs in
  help expanded

let print_rle_block ((_, sz, bl) : int * int * block) =
  (* Format.printf "@%d: %d x " lind sz;
     match bl with
     | File(id) -> Format.printf "File(%d)" id
     | Free     -> Format.printf "Free"
  *)
  (*Format.printf "[%d]" ind;*)
  List.iter
    (fun _ ->
      match bl with
      | File id -> Format.printf "%d" id
      | Free -> Format.printf ".")
    (List.init sz Fun.id)

let print_rle_blocks (l : (int * int * block) list) =
  l |> List.iter (fun b -> print_rle_block b (*Format.printf ", " *))

type indexed_run_block = int * int * block
type indexed_rle = indexed_run_block list

let rec remove_rle (l : indexed_rle) (indx : int) =
  match l with
  | ((i1, _, File _) as f1) :: rst when i1 < indx -> f1 :: remove_rle rst indx
  (* When the list starts with the target indx *)
  | (i1, sz1, File _) :: (_, sz2, Free) :: rst when i1 = indx ->
      (i1, sz1 + sz2, Free) :: rst
  | (i1, sz1, File _) :: ((_, _, File _) as f) :: rst when i1 = indx ->
      (i1, sz1, Free) :: f :: rst
  (*
      When the list has the target somewhere in the middle of the list
      Case where the list starts with a File is taken by case #1
  *)
  | (i1, sz1, Free) :: (i2, sz2, File _) :: (_, sz3, Free) :: rst when i2 = indx
    ->
      (i1, sz1 + sz2 + sz3, Free) :: rst
  | (i1, sz1, Free) :: (i2, sz2, File _) :: ((_, _, File _) as f) :: rst
    when i2 = indx ->
      (i1, sz1 + sz2, Free) :: f :: rst
  | ((_, _, Free) as f1) :: ((i2, _, File _) as f2) :: rst when i2 < indx ->
      f1 :: f2 :: remove_rle rst indx
  (* list ends with the target file *)
  | [ (i1, sz1, File _) ] when i1 = indx -> [ (i1, sz1, Free) ]
  | _ ->
      Format.printf "REMOVE ERROR: ";
      print_rle_blocks l;
      Format.printf " , target index: %d" indx;
      Format.printf "\n";
      raise NotImplemented

let rec add_rle (l : indexed_rle) ((ind, sz, _) as blk : indexed_run_block) :
    indexed_rle =
  match l with
  | (li, lsz, Free) :: rst ->
      if li = ind && ind + sz = li + lsz then
        blk :: rst
      else if li = ind && ind + sz < li + lsz then
        blk :: (ind + sz, sz - lsz, Free) :: rst
      else
        raise NotImplemented
  | x :: xs -> x :: add_rle xs blk
  | [] -> raise NotReachable

let rec file_sum ind max n =
  if ind = max then 0 else (ind * n) + file_sum (ind + 1) max n

let get_id (i : block) : int = match i with File id -> id | _ -> 0

let part_two (dm : string) : int =
  let rle = dm |> String.to_seq |> List.of_seq |> expand_into_rle in
  let expanded =
    rle
    |> List.fold_right (fun (f, b) (l, ind) -> ((ind - f, f, b) :: l, ind - f))
    |> ( |> ) ([], rle |> List.map fst |> List.fold_left ( + ) 0)
    |> fst
  in
  let reversed = expanded |> List.rev in
  (* print_blocks reordered; *)
  let rec reorder (forward : indexed_rle) (insert : indexed_run_block) :
      indexed_rle =
    match (insert, forward) with
    | _, [] -> []
    | (_, _, Free), _ -> raise NotImplemented
    | (i1, _, _), (i2, _, _) :: _ when i1 = i2 -> forward
    | _, ((_, _, File _) as f) :: rst -> f :: reorder rst insert
    | (i1, sz1, File f1), (i2, sz2, Free) :: rst when sz1 < sz2 ->
        (i2, sz1, File f1) :: (i2 + sz1, sz2 - sz1, Free) :: remove_rle rst i1
    | (i1, sz1, File f1), (i2, sz2, Free) :: rst when sz1 = sz2 ->
        (i2, sz1, File f1) :: remove_rle rst i1
    | ((_, _, File _) as f1), ((_, _, Free) as f2) :: rst (* sz1 > sz2 *) ->
        f2 :: reorder rst f1
  in
  reversed
  |> List.filter (fun (_, _, b) -> not (is_free b))
  |> List.fold_left
       (fun ex blk ->
         (*print_rle_blocks ex;
           Format.printf "\n";*)
         reorder ex blk)
       expanded
  |> List.map (fun (i, sz, b) -> (i, sz, get_id b))
  |> List.map (fun (i, sz, b) -> file_sum i (i + sz) b)
  |> List.fold_left ( + ) 0

let rec find_all_that_fit ?(acc = []) (sz : int) (l : indexed_rle) : indexed_rle
    =
  match l with
  | (_, _, Free) :: rst -> find_all_that_fit ~acc sz rst
  | ((_, lz, File _) as f) :: rst ->
      if lz <= sz then
        find_all_that_fit ~acc:(f :: acc) (sz - lz) rst
      else
        find_all_that_fit ~acc sz rst
  | [] -> List.rev acc

let part_two_wrong (dm : string) : int =
  let rle = dm |> String.to_seq |> List.of_seq |> expand_into_rle in
  let expanded : indexed_rle =
    rle
    |> List.fold_right (fun (f, b) (l, ind) -> ((ind - f, f, b) :: l, ind - f))
    |> ( |> ) ([], rle |> List.map fst |> List.fold_left ( + ) 0)
    |> fst
  in
  (* expanded: (int * int * block) *)
  let rec reorder (l : indexed_rle) : indexed_rle =
    match l with
    | (ind, sz, Free) :: rst ->
        let fits = find_all_that_fit sz (List.rev rst) in
        let new_blocks =
          fits
          |> List.fold_left
               (fun l (_, sz', b') ->
                 let where_to_add =
                   match List.nth l (List.length l - 1) with
                   | i, sz, _ -> i + sz
                 in
                 add_rle l (where_to_add, sz', b'))
               [ (ind, sz, Free) ]
        in
        let new_rst =
          List.fold_left remove_rle rst (fits |> List.map (fun (i, _, _) -> i))
        in
        new_blocks @ reorder new_rst
    | x :: xs -> x :: reorder xs
    | [] -> []
  in
  reorder expanded
  |> List.map (fun (ind, sz, f) -> file_sum ind (ind + sz) (get_id f))
  |> List.fold_left ( + ) 0

let parse_file (file_content : string) : string =
  file_content |> String.split_on_char '\n' |> List.hd
