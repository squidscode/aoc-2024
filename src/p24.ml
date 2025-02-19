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

let sort_connections (connl : connection list) : connection list option =
  (* topological_sort the ins *)
  let top = connl
  |> List.to_seq
  |> Seq.map (fun ({wfrom=(l,r);wto;_} : connection) -> List.to_seq [(l,wto) ; (r,wto)])
  |> Seq.concat
  |> adjacency_list 
  |> topological_sort
  in
  Option.bind top (fun top ->
    let c = connl
      |> List.sort (fun {wto=wto;_} {wto=wto';_} -> 
        Int.compare (wto |> (=) |> fun fn -> List.find_index fn top |> Option.get) 
          (wto' |> (=) |> fun fn -> List.find_index fn top |> Option.get) 
      ) in
    if List.length c = List.length connl then c |> Option.some else None
  )

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
(* assumes that (n-1) bit addition works correctly! *)
let zeros (n : int) : int StringMap.t = 
  let x = List.init n Fun.id
  |> List.map (fun i -> (Format.sprintf "x%02d" i, 0))
  |> StringMap.of_list in
  let y = List.init n Fun.id
  |> List.map (fun i -> (Format.sprintf "y%02d" i, 0))
  |> StringMap.of_list in
  StringMap.union (fun _ _ _ -> raise Not_found) x y

let zeroz (n : int) : int StringMap.t = 
  List.init n Fun.id
  |> List.map (fun i -> (Format.sprintf "z%02d" i, 0))
  |> StringMap.of_list

let ce (expected : int StringMap.t) (actual : int StringMap.t) : bool =
  (*
  Format.printf "ce test...\n";
  expected |> StringMap.iter ( Format.printf "%s %i; " ); Format.printf "\n";
  actual |> StringMap.filter (fun s _ -> String.get s 0 = 'z')
  |> StringMap.iter ( Format.printf "%s %i; " ); Format.printf "\n";
  Format.print_flush ();
  *)
  StringMap.to_seq expected
  |> Seq.fold_left (fun b (s,i) -> b && StringMap.find_opt s actual = Some i) true

let verify_carry_to_max (max_n : int) (connections : connection list) : bool =
  let zero = zeros max_n in
  let zeroz = zeroz max_n in
  let connections = sort_connections connections in
  if connections |> Option.is_none then false else 
  let connections = Option.get connections in
  (* carry over 0b10{n-2} + 0b10{n-2} =? 0b10{n-1} *)
  let zeroz' = zeroz |> StringMap.add (Format.sprintf "z%02d" (max_n-1)) 1 in
  let carry_test = 
    zero 
    |> StringMap.add (Format.sprintf "x%02d" (max_n-2)) 1
    |> StringMap.add (Format.sprintf "y%02d" (max_n-2)) 1
    |> simulate_wires connections
    |> ce zeroz'
  in
  Format.printf "%d-bit adder carry test: %b\n" max_n carry_test;
  carry_test

let verify_n_bit_addition (max_n : int) (connections : connection list) (n : int) : bool =
  let zero = zeros max_n in
  let zeroz = zeroz n in
  let connections = sort_connections connections in
  if connections |> Option.is_none then false else 
  let connections = Option.get connections in
  (*
  zero |> StringMap.iter (Format.printf "%s %i; "); Format.printf "\n";
  Format.print_flush ();
  *)
  (* carry over 0b10{n-2} + 0b10{n-2} =? 0b10{n-1} *)
  let zeroz' = zeroz |> StringMap.add (Format.sprintf "z%02d" (n-1)) 1 in
  let carry_test = 
  if n <= 1 then true
  else
    zero 
    |> StringMap.add (Format.sprintf "x%02d" (n-2)) 1
    |> StringMap.add (Format.sprintf "y%02d" (n-2)) 1
    |> simulate_wires connections
    |> ce zeroz'
  in
  (* one bit on 0b10{n-1} + 0 = 0b10{n-1} *)
  let zeroz' = zeroz |> StringMap.add (Format.sprintf "z%02d" (n-1)) 1 in
  let lbit_on = zero 
  |> StringMap.add (Format.sprintf "x%02d" (n-1)) 1
  |> simulate_wires connections
  |> ce zeroz'
  in
  (* one bit on 0 + 0b10{n-1} = 0b10{n-1} *)
  let zeroz' = zeroz |> StringMap.add (Format.sprintf "z%02d" (n-1)) 1 in
  let rbit_on = zero 
  |> StringMap.add (Format.sprintf "y%02d" (n-1)) 1
  |> simulate_wires connections
  |> ce zeroz'
  in
  (* one bit on 0b11{n-1} + 0b01{n-1} = 0b00{n-1} *)
  let zeroz' = zeroz in
  let xcarry =
  if n <= 1 then true
  else zero 
  |> StringMap.add (Format.sprintf "x%02d" (n-1)) 1
  |> StringMap.add (Format.sprintf "x%02d" (n-2)) 1
  |> StringMap.add (Format.sprintf "y%02d" (n-2)) 1
  |> simulate_wires connections
  |> ce zeroz'
  in
  (* one bit on 0b01{n-1} + 0b11{n-1} = 0b10{n-1} *)
  let zeroz' = zeroz in
  let ycarry =
  if n <= 1 then true
  else zero 
  |> StringMap.add (Format.sprintf "x%02d" (n-2)) 1
  |> StringMap.add (Format.sprintf "y%02d" (n-1)) 1
  |> StringMap.add (Format.sprintf "y%02d" (n-2)) 1
  |> simulate_wires connections
  |> ce zeroz'
  in
  (* one bit on 0b10{n-1} + 0b10{n-1} = 0b?0{n} *)
  let zeroz' = List.init (max_n - n) ((+) n)
  |> List.fold_left (fun smp n -> StringMap.remove (Format.sprintf "z%02d" n) smp) zeroz in
  let both_on = zero 
  |> StringMap.add (Format.sprintf "x%02d" (n-1)) 1
  |> StringMap.add (Format.sprintf "y%02d" (n-1)) 1
  |> simulate_wires connections
  |> ce zeroz'
  in
  Format.printf "%d-bit adder carry test: %b\n" n carry_test;
  Format.printf "%d-bit adder xcarry test: %b\n" n xcarry;
  Format.printf "%d-bit adder ycarry test: %b\n" n ycarry;
  Format.printf "%d-bit adder lbit test: %b\n" n lbit_on;
  Format.printf "%d-bit adder rbit test: %b\n" n rbit_on;
  Format.printf "%d-bit adder and test: %b\n" n both_on;
  carry_test && lbit_on && rbit_on && both_on && xcarry && ycarry

(* for some list of vertices, find all vertices that depend on them *)
let dependencies (s : StringSet.t) (connl : connection list) : StringSet.t =
  let rev_conn_adj = connl
    |> List.map (fun {wfrom=(f1, f2); wto=(t); _} -> [(f1,t);(f2,t)]) 
    |> List.flatten
    |> List.map (fun (f,t) -> (t,f))
    |> List.fold_left (fun assoc ((f,t) : (string*string)) ->
      List.assoc_opt f assoc
      |> (fun l -> t::(Option.value ~default:[] l))
      |> (fun l -> (f,l))
      |> Fun.flip List.cons (List.remove_assoc f assoc)
    ) []
  in
  let rec dep_help (s : string) : StringSet.t =
    List.assoc_opt s rev_conn_adj
    |> Option.value ~default:[]
    |> List.map dep_help
    |> List.fold_left StringSet.union StringSet.empty
    |> StringSet.add s
  in
  s
  |> StringSet.to_seq
  |> Seq.map dep_help
  |> Seq.fold_left StringSet.union StringSet.empty

let powerset (ss : StringSet.t) : StringSet.t Seq.t =
  let rec help (sl : string list) : StringSet.t Seq.t = 
    match sl with
    | [] -> [StringSet.empty] |> List.to_seq
    | f::rst -> 
      let r = help rst in
      Seq.append r
      (r |> Seq.map (StringSet.add f))
  in
  StringSet.to_list ss
  |> help

let rec generate_swaps (ss : StringSet.t) (ins : string list) : (string * string) list Seq.t =
  match ins with
  | [] -> [[]] |> List.to_seq
  | f::rst -> 
    let rst = generate_swaps ss rst in
    ss
    |> StringSet.to_seq
    |> Seq.map (fun s -> (f,s))
    |> Seq.map (fun p ->
      rst |> Seq.map (fun l -> p::l)
    )
    |> Seq.flat_map Fun.id

let rec get_connection (connl : connection list) (driver : string) =
  match connl with
  | [] -> None
  | ({wto=f;_} as conn :: _) when f=driver -> conn |> Option.some
  | _ :: rst -> get_connection rst driver

let rec apply_swaps (connl : connection list) (swaps : (string * string) list) 
  : connection list option =
  match swaps with
  | [] -> connl |> Option.some
  | (l,r)::rst -> 
    let lc = get_connection connl l |> Option.bind in
    let rc = get_connection connl r |> Option.bind in
    (* bind optional connections *)
    lc (fun lc -> (rc (fun rc ->
    let new_connl = connl
    |> List.filter (fun {wto=t;_} -> t <> l && t <> r) in
    let new_connl = {wfrom=lc.wfrom;wto=r;operation=lc.operation} ::
      {wfrom=rc.wfrom;wto=l;operation=rc.operation} :: new_connl in
    apply_swaps new_connl rst
    )))

let print_string_set (ss : StringSet.t) : unit =
  Format.printf "{";
  ss
  |> StringSet.iter (Format.printf "%s, ");
  Format.printf "}"

let rec solve ?(swaps = []) ?(max_swaps=4) (n : int) (ind : int) (connl : connection list) 
  : (string * string) list option =
    if n = ind then 
      if verify_carry_to_max n connl then
        swaps |> Option.some
      else
        None
    else
    connl |> sort_connections |> Fun.flip Option.bind (fun connl ->
      if (verify_n_bit_addition n connl ind) then
        (* valid ind-bit adder constructed, move on to larger adder *)
        solve ~swaps n (ind+1) connl
      else
        (* invalid ind-bit adder, but valid (ind-1)-bit adder *)
        let pre_zs = List.init (ind-1) Fun.id
          |> List.map (Format.sprintf "z%02d")
          |> StringSet.of_list 
        in
        (* some sort of heuristic to minimize the search space *)
        let post_zs = List.init (Int.min 2 (n - ind)) ((+) (ind - 1)) 
          |> List.map (Format.sprintf "z%02d")
          |> StringSet.of_list
        in
        Format.printf "\n\nINVALID %d-bit adder!\n" ind;
        let predeps = dependencies pre_zs connl in
        let postdeps = dependencies post_zs connl 
          |> (fun l -> StringSet.diff l predeps) 
        in
        Format.printf "pre-deps: "; print_string_set predeps; Format.printf "\n";
        Format.printf "post-deps: "; print_string_set postdeps; Format.printf "\n";
        postdeps
        |> powerset
        |> Seq.map StringSet.to_list
        |> Seq.filter (fun l -> List.length l + List.length swaps <= max_swaps)
        |> Seq.map (generate_swaps postdeps)
        |> Seq.flat_map Fun.id
        |> Seq.map (fun s -> (swaps@s, apply_swaps connl s))
        |> Seq.filter (fun (_,connl) -> connl |> Option.is_some)
        |> Seq.map (fun (l,connl) -> (l,connl |> Option.get))
        |> Seq.filter (fun (swaps,_) ->
          swaps 
          |> List.map (fun (l,r) -> [l;r]) 
          |> List.flatten
          |> List.sort_uniq String.compare 
          |> List.length
          |> ((=) (List.length swaps * 2))
        )
        |> Seq.filter (fun (_s,connl) -> 
          Format.printf "testing %d-bit validity: " ind;
          _s
          |> List.iter (fun (l,r) -> Format.printf "(%s,%s), " l r);
          Format.printf "\n"; Format.print_flush ();
          verify_n_bit_addition n connl ind
          && 
          (
            List.init (ind) ((+) 1)
            |> List.fold_left (fun b v ->
              b && verify_n_bit_addition n connl v
            ) true
          )
        )
        |> Seq.map (fun (swaps',connl) ->
          Format.printf "VALID SWAP CONFIG: ";
          swaps'
          |> List.iter (fun (l,r) -> Format.printf "(%s,%s), " l r);
          Format.printf "\n"; Format.print_flush ();
          let ans = solve ~swaps:swaps' n (ind+1) connl in
          if Option.is_some ans then ans else begin
            Format.printf "BRANCH FAILED [%d-bit adder]\n\n" ind;
            ans
          end
        )
        |> Seq.find (Option.is_some)
        |> Option.join
    )

let part_two ((initl, connl) : config) : int =
  let connl = connl |> sort_connections |> Option.get in
  Printexc.record_backtrace true;
  (* The circuit is an adder of two n x n bit numbers, producing an n+1 bit number *)
  let n = initl |> List.length |> fun x -> (/) x 2 in
  solve n 1 connl
  |> Option.get 
  |> List.map (fun (s,s') -> [s;s'])
  |> List.flatten
  |> List.sort_uniq String.compare
  |> List.iter (Format.printf "%s,");
  Format.printf "\n";
  0


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
