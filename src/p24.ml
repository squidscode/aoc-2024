(* 
 *  Solves Advent of Code Problem 24:
 *  https://adventofcode.com/2024/day/24
 *)

type init = { (* short for initialization *)
  wire: string;
  binary: int; (* 0 or 1 ; we represent this as int because I'm lazy *)
}

type operation = 
  | OR 
  | AND
  | XOR 

let operation_of_string operation =
  match operation with
  | "OR" -> OR
  | "AND" -> AND
  | "XOR" -> XOR
  | _ -> 
    Format.printf "operation not found: %s\n" operation;
    raise Not_found

let operation_to_fn operation =
  match operation with
  | OR  -> Int.logor
  | AND -> Int.logand
  | XOR -> Int.logxor

type connection = {
  (* wire-from and wire-to *)
  wfrom: (string * string);
  operation: operation;
  wto: string;
}

type config = (init list * connection list)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)


let add_edge  (adj : string list StringMap.t) 
  ((sfrom, sto) : (string * string)) : string list StringMap.t =
  let adjl = StringMap.find_opt sfrom adj |> Option.value ~default:[] in
  if List.mem sto adjl then adj else
  StringMap.add sfrom (sto :: adjl) adj

let adjacency_list (edges : (string * string) Seq.t) : string list StringMap.t =
  let nodes = edges 
  |> Seq.map (fun (s1, s2) -> List.to_seq [s1 ; s2]) 
  |> Seq.concat
  |> Seq.map (fun s -> (s, []))
  |> StringMap.of_seq
  in
  edges |> Seq.fold_left add_edge nodes

let edges (adj : string list StringMap.t) : (string * string) Seq.t =
  StringMap.to_seq adj 
  |> Seq.map (fun (f, ts) -> ts 
    |> List.to_seq
    |> Seq.map (fun t -> (f, t)))
  |> Seq.flat_map Fun.id

let print_in_degrees (in_deg : int StringMap.t) : unit =
  Format.printf "smap: ";
  in_deg |> StringMap.iter (fun s i -> Format.printf "(%s, %d), " s i);
  Format.printf "\n"

let topological_sort (adj : string list StringMap.t) : string list option =
  let in_deg = edges adj 
    |> Seq.map (fun (f,t) -> (t,f)) 
    |> adjacency_list 
    |> StringMap.map List.length 
  in
  let rec help (in_deg : int StringMap.t) : string list =
    match StringMap.to_seq in_deg |> Seq.find (fun (_, i) -> i = 0) with
    | None ->        []
    | Some((s,_)) -> 
      let in_deg = in_deg
        |> StringMap.remove s in
      let in_deg = StringMap.find_opt s adj |> Option.value ~default:[] 
        |> List.fold_left (fun sm sto -> 
          StringMap.add sto (StringMap.find sto sm - 1) sm
        ) in_deg in
      s :: (help in_deg)
  in
  let l = help in_deg in
  if List.length l = StringMap.cardinal adj then
    l |> Option.some
  else
    None

let rec simulate_wires (connections : connection list) (charges : int StringMap.t) 
  : int StringMap.t =
  match connections with 
  | [] -> charges
  | {wfrom=(l,r);wto;operation} :: rst ->
    let l = StringMap.find l charges in
    let r = StringMap.find r charges in
    StringMap.add wto ((operation |> operation_to_fn) l r) charges
    |> simulate_wires rst

let part_one ((initl, connl) : config) : int = 
  List.length initl |> Format.printf "initl length: %d\n";
  List.length connl |> Format.printf "connl length: %d\n\n";


  (* topological_sort the ins *)
  let top = connl
    |> List.to_seq
    |> Seq.map (fun ({wfrom=(l,r);wto;_} : connection) -> List.to_seq [(l,wto) ; (r,wto)])
    |> Seq.concat
    |> adjacency_list 
    |> topological_sort
    |> Option.get
  in

  (* print topo sort *)
  Format.printf "topological_sort: ";
  top
    |> List.iter (Format.printf "%s, ");
  Format.printf "\n";

  let sorted_connl = connl
    |> List.sort (fun {wto=wto;_} {wto=wto';_} -> 
      Int.compare (wto |> (=) |> fun fn -> List.find_index fn top |> Option.get) 
        (wto' |> (=) |> fun fn -> List.find_index fn top |> Option.get) 
    ) in
  let initial_charges = initl
    |> List.map (fun {wire;binary} -> (wire,binary))
    |> StringMap.of_list
  in

  let final = simulate_wires sorted_connl initial_charges
    |> StringMap.to_list
    |> List.filter (fun (s, _) -> String.get s 0 = 'z')
    |> List.sort (fun (s,_) (s',_) -> String.compare s s')
    |> List.rev
  in

  final |> List.iter (fun (s, i) -> Format.printf "\n(%s, %d), " s i);
  Format.printf "\n\n";

  final
    |> List.map snd
    |> List.fold_left (fun acc i -> 2*acc + i) 0

(**
 * Checking for correctness is basically impossible, given that there are
 * 128,795,283,000,000 possible eight pairs... in other words, we cannot 
 * generate all possible configurations of gates and check if the program is 
 * valid.

 * We can, however, make a statement about how many of these swaps are possible to 
 * topological sort.

 * We can topological sort a gate configuration IFF there are no cycles in the graph.

 * Presumably, some of the program configurations will cause a cycle to arise, and 
 * we can eliminate these programs from the search space.

 *)
let part_two (_ : config) : int = 0 (* DELETE ME *)

(* x00: 1 *)
let initialization_re = Re.Perl.re "(\\w+): (\\d)" |> Re.Perl.compile

(* ntg XOR / OR / AND fgs -> mjb *)
let wire_re = Re.Perl.re "(\\w+) (\\w+) (\\w+) -> (\\w+)" |> Re.Perl.compile

let parse_file (file_content : string) : config =
  let lb = file_content 
  |> String.split_on_char '\n'
  |> List.find_index ((=) "")
  |> Option.get in
  let lines = file_content
  |> String.split_on_char '\n'
  |> List.to_seq in
  let initial_wires = lines
  |> Seq.take lb
  |> Seq.map (Re.all initialization_re)
  |> Seq.map List.hd
  |> Seq.map (fun g -> {
    wire=(Re.Group.get g 1); binary=(Re.Group.get g 2 |> int_of_string)
  })
  |> List.of_seq in
  let connection_wires = lines
  |> Seq.drop (lb + 1)
  |> Seq.filter ((<>) "")
  |> Seq.map (Re.all wire_re)
  |> Seq.map List.hd
  |> Seq.map (fun g -> {
    wfrom=(Re.Group.get g 1, Re.Group.get g 3);
    operation=(Re.Group.get g 2 |> operation_of_string);
    wto=(Re.Group.get g 4)
  }) 
  |> List.of_seq in
  (initial_wires, connection_wires)
