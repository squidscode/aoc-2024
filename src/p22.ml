(* 
 *  Solves Advent of Code Problem 22:
 *  https://adventofcode.com/2024/day/22
 *)

let p = 16777216

let mix ((a, b) : (int * int)) : int = Int.logxor a b

let prune (p : int) (a : int) : int = a mod p

let rec simulate (n : int) (init : int) : int =
  (* Format.printf "simulate %d %d\n" n init;*)
  if n <= 0 then init else
  init
  (* STEP 1 *)
  |> fun i -> (i, 64 * i)
  |> mix |> prune p
  (* STEP 2 *)
  |> fun i -> (i, i / 32)
  |> mix |> prune p
  (* STEP 3 *)
  |> fun i -> (i, i * 2048)
  |> mix |> prune p
  (* finally simulate the rest *)
  |> simulate (n-1)

let part_one (secrets : int list) : int =
  secrets
  |> List.map (simulate 2000)
  |> List.fold_left (+) 0


module Changes = struct
  type t = (int * int * int * int)
  let equal (a : t) (b : t) = a = b
  let hash ((a, b, c, d) : t): int = 
    a + 31 * b + 101 * c + 2017 * d
end
module ChangeTbl = Hashtbl.Make(Changes)

let print_changetbl tbl =
  tbl |> ChangeTbl.iter (fun (f, s, t, fo) price ->
    Format.printf "(%d %d %d %d) -> %d\n" f s t fo price
  );
  Format.printf "\n"

let l = 4

let price v = v mod 10
let diff a b = (a mod 10) - (b mod 10)

let rec simulate' (acc : int list) (prev : int) (tbl : int ChangeTbl.t)
  (n : int) (init : int) : unit =
  let k = match acc with 
  | [a;b;c;d] -> 
    (* Format.printf "(%d, %d, %d, %d) " a b c d;*)
    (a,b,c,d)
  | _ -> raise Not_found
  in
  (* Format.printf "n = %d ; init = %d, " n init;*)
  if n = 0 then
    (*Format.printf "\n"*)
    ()
  else
  let acc = match acc with 
  | _ :: rst when List.length acc = 4 -> rst @ [diff init prev]
  | _ -> raise Not_found
  in
  if ChangeTbl.mem tbl k then begin
    (*Format.printf "HIT\n"; 
    Format.print_flush ();*)
    simulate' acc init tbl (n-1) (simulate 1 init) 
  end else begin
    (*Format.printf "MISS ; adding key = %d\n" (prev |> price); 
    Format.print_flush ();*)
    ChangeTbl.add tbl k (prev |> price);
    simulate' acc init tbl (n-1) (simulate 1 init)
  end
  

let part_two (secrets : int list) : int = 
  secrets
  |> List.mapi (fun i s -> 
    Format.printf "Solving for #%d :: s = %d\n" (i+1) s;
    Format.print_flush ();
    let tbl = ChangeTbl.create 10000 in
    let l = [
      diff (simulate 1 s) (simulate 0 s) ;
      diff (simulate 2 s) (simulate 1 s) ;
      diff (simulate 3 s) (simulate 2 s) ; 
      diff (simulate 4 s) (simulate 3 s) ] in
    simulate' l (simulate 4 s) tbl (2000) (simulate 5 s);
    tbl
  )
  |> List.fold_left (fun acc tbl -> 
    tbl
    |> ChangeTbl.iter (fun k v ->
      if not @@ ChangeTbl.mem acc k then
        ChangeTbl.add acc k v 
      else begin
        let v' = ChangeTbl.find acc k in
        ChangeTbl.add acc k (v + v')
      end
    );
    acc
  ) (ChangeTbl.create 2000)
  |> ChangeTbl.to_seq_values
  |> Seq.fold_left Int.max Int.min_int

let parse_file (file_content : string) : int list =
  file_content 
  |> String.split_on_char '\n'
  |> List.filter ((<>) "")
  |> List.map int_of_string
