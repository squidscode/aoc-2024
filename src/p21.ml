(* 
 *  Solves Advent of Code Problem 21:
 *  https://adventofcode.com/2024/day/21
 *)

type numeric_key = char
type directional_key = char

(* 
  NUMERIC KEYPAD
  +---+---+---+
  | 7 | 8 | 9 |
  +---+---+---+
  | 4 | 5 | 6 |
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
      | 0 | A |
      +---+---+

  DIRECTIONAL KEYPAD
      +---+---+
      | ^ | A |
  +---+---+---+
  | < | v | > |
  +---+---+---+
 *)

let numerical = "789\n456\n123\n 0A" |> String.split_on_char '\n' |> Position.get_pmap
let directional = " ^A\n<v>" |> String.split_on_char '\n' |> Position.get_pmap

let rec print_path (path : directional_key list) : unit =
  match path with
  | [] -> ()
  | f :: [] -> Format.printf "'%c'" f
  | f :: rst -> Format.printf "'%c'; " f; print_path rst

let rec print_paths (paths : directional_key list list) : unit =
  match paths with
  | [] -> ()
  | f :: [] -> 
    Format.printf "[";
    print_path f;
    Format.printf "]"
  | f :: rst -> 
    Format.printf "[";
    print_path f;
    Format.printf "]";
    Format.printf "; ";
    print_paths rst


let get_adjs' (p : Position.position) : (Position.position * directional_key) list =
  [
    ({x=p.x + 1 ; y=p.y}, '<');
    ({x=p.x - 1 ; y=p.y}, '>');
    ({x=p.x ; y=p.y + 1}, '^');
    ({x=p.x ; y=p.y - 1}, 'v')
  ]


let rec get_paths (pto : Position.position) (mp : int Position.PositionMap.t) 
  : directional_key list list =
  let dist = Position.PositionMap.find pto mp in
  let dirs = pto
    |> get_adjs'
    |> List.filter (fun (p, _) -> Position.PositionMap.mem p mp)
    |> List.filter (fun (p, _) -> Position.PositionMap.find p mp = dist - 1)
  in
  if dist = 1 then begin
    assert (List.length dirs = 1);
    [dirs |> List.map (fun (_, k) -> k)]
  end else
    dirs 
      |> List.map (fun (p, k) ->
        get_paths p mp
        |> List.map (fun l -> l @ [k])
      )
      |> List.flatten
  

let print_num_distances () : unit =
  let chars : char Seq.t = "1234567890A" |> String.to_seq in
  let obstacles = {x=0;y=3} |> Position.PositionSet.singleton in
  chars
  |> Seq.map (fun (c : char) -> (Position.find_position numerical c |> Option.get, 0)) 
  |> Seq.map (fun p -> (p, Position.PositionDistanceSet.singleton p))
  |> Seq.map (fun (p, s) -> (p, Position.dijkstras 3 4 obstacles s))
  |> Seq.iter 
  (fun (((pfrom, _), s) : Position.Iposdist.t * int Position.PositionMap.t) -> 
      s |> Position.PositionMap.to_seq |> Seq.iter 
      (fun ((pto, _) : Position.Iposdist.t) -> 
        Format.printf "| ('%c', '%c') -> " 
          (Position.PositionMap.find pfrom numerical)
          (Position.PositionMap.find pto numerical);
        Format.printf "[";
        get_paths pto s |> print_paths;
        Format.printf "]";
        Format.printf "\n"
      )
  )

let print_directional_distances () : unit =
  let chars : char Seq.t = "<>^vA" |> String.to_seq in
  let obstacles = {x=0;y=0} |> Position.PositionSet.singleton in
  chars
  |> Seq.map (fun (c : char) -> (Position.find_position directional c |> Option.get, 0)) 
  |> Seq.map (fun p -> (p, Position.PositionDistanceSet.singleton p))
  |> Seq.map (fun (p, s) -> (p, Position.dijkstras 3 2 obstacles s))
  |> Seq.iter 
  (fun (((pfrom, _), s) : Position.Iposdist.t * int Position.PositionMap.t) -> 
      s |> Position.PositionMap.to_seq |> Seq.iter 
      (fun ((pto, _) : Position.Iposdist.t) -> 
        Format.printf "| ('%c', '%c') -> " 
          (Position.PositionMap.find pfrom directional)
          (Position.PositionMap.find pto directional);
        Format.printf "[";
        get_paths pto s |> print_paths;
        Format.printf "]";
        Format.printf "\n"
      )
  )


let num_distance (fromk : numeric_key) (tok : numeric_key) 
  : directional_key list list =
  match (fromk, tok) with
    | ('1', '7') -> [['^'; '^']]
    | ('1', '4') -> [['^']]
    | ('1', '1') -> []
    | ('1', '8') -> [
      ['^'; '^'; '>']; 
      (* ['^'; '>'; '^']; the one above is always better than this one *)
      ['>'; '^'; '^']
    ]
    | ('1', '5') -> [['^'; '>']; ['>'; '^']]
    | ('1', '2') -> [['>']]
    | ('1', '0') -> [['>'; 'v']]
    | ('1', '9') -> [
      ['^'; '^'; '>'; '>']; 
      (* ['^'; '>'; '^'; '>']; *)
      (* ['>'; '^'; '^'; '>']; *)
      (* ['^'; '>'; '>'; '^']; above is better *)
      (* ['>'; '^'; '>'; '^']; below is better *)
      ['>'; '>'; '^'; '^']
    ]
    | ('1', '6') -> [
      ['^'; '>'; '>']; 
      (* ['>'; '^'; '>']; *)
      ['>'; '>'; '^']
    ]
    | ('1', '3') -> [['>'; '>']]
    | ('1', 'A') -> [['>'; 'v'; '>']; ['>'; '>'; 'v']]
    | ('2', '7') -> [
      ['^'; '^'; '<']; 
      (* ['^'; '<'; '^'];  *)
      ['<'; '^'; '^']
    ]
    | ('2', '4') -> [['^'; '<']; ['<'; '^']]
    | ('2', '1') -> [['<']]
    | ('2', '8') -> [['^'; '^']]
    | ('2', '5') -> [['^']]
    | ('2', '2') -> []
    | ('2', '0') -> [['v']]
    | ('2', '9') -> [
      ['^'; '^'; '>']; 
      (* ['^'; '>'; '^'];  *)
      ['>'; '^'; '^']
    ]
    | ('2', '6') -> [['^'; '>']; ['>'; '^']]
    | ('2', '3') -> [['>']]
    | ('2', 'A') -> [['v'; '>']; ['>'; 'v']]
    | ('3', '7') -> [
      ['^'; '^'; '<'; '<']; 
      (* ['^'; '<'; '^'; '<']; *)
      (* ['<'; '^'; '^'; '<']; *)
      (* ['^'; '<'; '<'; '^'];  *)
      (* ['<'; '^'; '<'; '^'];  *)
      ['<'; '<'; '^'; '^']
    ]
    | ('3', '4') -> [
      ['^'; '<'; '<']; 
      (* ['<'; '^'; '<']; *)
      ['<'; '<'; '^']
    ]
    | ('3', '1') -> [['<'; '<']]
    | ('3', '8') -> [
      ['^'; '^'; '<']; 
      (* ['^'; '<'; '^']; *)
      ['<'; '^'; '^']
    ]
    | ('3', '5') -> [['^'; '<']; ['<'; '^']]
    | ('3', '2') -> [['<']]
    | ('3', '0') -> [['v'; '<']; ['<'; 'v']]
    | ('3', '9') -> [['^'; '^']]
    | ('3', '6') -> [['^']]
    | ('3', '3') -> []
    | ('3', 'A') -> [['v']]
    | ('4', '7') -> [['^']]
    | ('4', '4') -> []
    | ('4', '1') -> [['v']]
    | ('4', '8') -> [['^'; '>']; ['>'; '^']]
    | ('4', '5') -> [['>']]
    | ('4', '2') -> [['v'; '>']; ['>'; 'v']]
    | ('4', '0') -> [['v'; '>'; 'v']; ['>'; 'v'; 'v']]
    | ('4', '9') -> [
      ['^'; '>'; '>']; 
      (* ['>'; '^'; '>']; *)
      ['>'; '>'; '^']
    ]
    | ('4', '6') -> [['>'; '>']]
    | ('4', '3') -> [
      ['v'; '>'; '>']; 
      (* ['>'; 'v'; '>'];  *)
      ['>'; '>'; 'v']
    ]
    | ('4', 'A') -> [
      (* ['v'; '>'; 'v'; '>'];  *)
      ['>'; 'v'; 'v'; '>']; 
      (* ['v'; '>'; '>'; 'v'];  *)
      (* ['>'; 'v'; '>'; 'v'];  *)
      ['>'; '>'; 'v'; 'v']
    ]
    | ('5', '7') -> [['^'; '<']; ['<'; '^']]
    | ('5', '4') -> [['<']]
    | ('5', '1') -> [['v'; '<']; ['<'; 'v']]
    | ('5', '8') -> [['^']]
    | ('5', '5') -> []
    | ('5', '2') -> [['v']]
    | ('5', '0') -> [['v'; 'v']]
    | ('5', '9') -> [['^'; '>']; ['>'; '^']]
    | ('5', '6') -> [['>']]
    | ('5', '3') -> [['v'; '>']; ['>'; 'v']]
    | ('5', 'A') -> [
      ['v'; 'v'; '>']; 
      (* ['v'; '>'; 'v']; *)
      ['>'; 'v'; 'v']
    ]
    | ('6', '7') -> [
      ['^'; '<'; '<']; 
      (* ['<'; '^'; '<']; *)
      ['<'; '<'; '^']
    ]
    | ('6', '4') -> [['<'; '<']]
    | ('6', '1') -> [['v'; '<'; '<']; (* ['<'; 'v'; '<']; *) ['<'; '<'; 'v']]
    | ('6', '8') -> [['^'; '<']; ['<'; '^']]
    | ('6', '5') -> [['<']]
    | ('6', '2') -> [['v'; '<']; ['<'; 'v']]
    | ('6', '0') -> [['v'; 'v'; '<']; (* ['v'; '<'; 'v']; *) ['<'; 'v'; 'v']]
    | ('6', '9') -> [['^']]
    | ('6', '6') -> []
    | ('6', '3') -> [['v']]
    | ('6', 'A') -> [['v'; 'v']]
    | ('7', '7') -> []
    | ('7', '4') -> [['v']]
    | ('7', '1') -> [['v'; 'v']]
    | ('7', '8') -> [['>']]
    | ('7', '5') -> [['v'; '>']; ['>'; 'v']]
    | ('7', '2') -> [['v'; 'v'; '>']; (* ['v'; '>'; 'v']; *) ['>'; 'v'; 'v']]
    | ('7', '0') -> [
      (*
      ['v'; 'v'; '>'; 'v']; 
      ['v'; '>'; 'v'; 'v']; 
      *)
      ['>'; 'v'; 'v'; 'v']
    ]
    | ('7', '9') -> [['>'; '>']]
    | ('7', '6') -> [['v'; '>'; '>']; (* ['>'; 'v'; '>']; *) ['>'; '>'; 'v']]
    | ('7', '3') -> [
      ['v'; 'v'; '>'; '>']; 
      (*
      ['v'; '>'; 'v'; '>']; 
      ['>'; 'v'; 'v'; '>']; 
      ['v'; '>'; '>'; 'v']; 
      ['>'; 'v'; '>'; 'v'];
      *)
      ['>'; '>'; 'v'; 'v']
    ]
    | ('7', 'A') -> [
      (*
      ['v'; 'v'; '>'; 'v'; '>']; 
      ['v'; '>'; 'v'; 'v'; '>']; 
      ['>'; 'v'; 'v'; 'v'; '>']; 
      ['v'; 'v'; '>'; '>'; 'v'];
      ['v'; '>'; 'v'; '>'; 'v']; 
      ['>'; 'v'; 'v'; '>'; 'v']; 
      ['v'; '>'; '>'; 'v'; 'v']; 
      ['>'; 'v'; '>'; 'v'; 'v']; 
      *)
      ['>'; '>'; 'v'; 'v'; 'v']
    ]
    | ('8', '7') -> [['<']]
    | ('8', '4') -> [['v'; '<']; ['<'; 'v']]
    | ('8', '1') -> [['v'; 'v'; '<']; (* ['v'; '<'; 'v']; *) ['<'; 'v'; 'v']]
    | ('8', '8') -> []
    | ('8', '5') -> [['v']]
    | ('8', '2') -> [['v'; 'v']]
    | ('8', '0') -> [['v'; 'v'; 'v']]
    | ('8', '9') -> [['>']]
    | ('8', '6') -> [['v'; '>']; ['>'; 'v']]
    | ('8', '3') -> [['v'; 'v'; '>']; (* ['v'; '>'; 'v']; *) ['>'; 'v'; 'v']]
    | ('8', 'A') -> [
      ['v'; 'v'; 'v'; '>']; 
      (*
      ['v'; 'v'; '>'; 'v']; 
      ['v'; '>'; 'v'; 'v']; 
      *)
      ['>'; 'v'; 'v'; 'v']
    ]
    | ('9', '7') -> [['<'; '<']]
    | ('9', '4') -> [['v'; '<'; '<']; (*['<'; 'v'; '<'];*) ['<'; '<'; 'v']]
    | ('9', '1') -> [
      ['v'; 'v'; '<'; '<']; 
      (*
      ['v'; '<'; 'v'; '<']; 
      ['<'; 'v'; 'v'; '<']; 
      ['v'; '<'; '<'; 'v']; 
      ['<'; 'v'; '<'; 'v']; 
      *)
      ['<'; '<'; 'v'; 'v']
    ]
    | ('9', '8') -> [['<']]
    | ('9', '5') -> [['v'; '<']; ['<'; 'v']]
    | ('9', '2') -> [['v'; 'v'; '<']; (*['v'; '<'; 'v'];*) ['<'; 'v'; 'v']]
    | ('9', '0') -> [
      ['v'; 'v'; 'v'; '<']; 
      (*
      ['v'; 'v'; '<'; 'v'];
      ['v'; '<'; 'v'; 'v']; 
      *)
      ['<'; 'v'; 'v'; 'v']
      ]
    | ('9', '9') -> []
    | ('9', '6') -> [['v']]
    | ('9', '3') -> [['v'; 'v']]
    | ('9', 'A') -> [['v'; 'v'; 'v']]
    | ('0', '7') -> [
      ['^'; '^'; '^'; '<']; 
      (*
      ['^'; '^'; '<'; '^']; 
      ['^'; '<'; '^'; '^']
      *)
    ]
    | ('0', '4') -> [['^'; '^'; '<']; ['^'; '<'; '^']]
    | ('0', '1') -> [['^'; '<']]
    | ('0', '8') -> [['^'; '^'; '^']]
    | ('0', '5') -> [['^'; '^']]
    | ('0', '2') -> [['^']]
    | ('0', '0') -> []
    | ('0', '9') -> [['^'; '^'; '^'; '>']; 
      (*
      ['^'; '^'; '>'; '^']; 
      ['^'; '>'; '^'; '^']; 
      *)
      ['>'; '^'; '^'; '^']
    ]
    | ('0', '6') -> [['^'; '^'; '>']; (*['^'; '>'; '^'];*) ['>'; '^'; '^']]
    | ('0', '3') -> [['^'; '>']; ['>'; '^']]
    | ('0', 'A') -> [['>']]
    | ('A', '7') -> [
      ['^'; '^'; '^'; '<'; '<']; 
      (*
      ['^'; '^'; '<'; '^'; '<']; 
      ['^'; '<'; '^'; '^'; '<']; 
      ['<'; '^'; '^'; '^'; '<']; 
      ['^'; '^'; '<'; '<'; '^']; 
      ['^'; '<'; '^'; '<'; '^']; 
      ['<'; '^'; '^'; '<'; '^']; 
      ['^'; '<'; '<'; '^'; '^']; 
      ['<'; '^'; '<'; '^'; '^']
      *)
    ]
    | ('A', '4') -> [
      ['^'; '^'; '<'; '<']; 
      (*
      ['^'; '<'; '^'; '<']; 
      ['<'; '^'; '^'; '<']; 
      ['^'; '<'; '<'; '^']; 
      ['<'; '^'; '<'; '^']
      *)
    ]
    | ('A', '1') -> [['^'; '<'; '<']; ['<'; '^'; '<']]
    | ('A', '8') -> [
      ['^'; '^'; '^'; '<']; 
      (*
      ['^'; '^'; '<'; '^']; 
      ['^'; '<'; '^'; '^']; 
      *)
      ['<'; '^'; '^'; '^']
    ]
    | ('A', '5') -> [['^'; '^'; '<']; (*['^'; '<'; '^'];*) ['<'; '^'; '^']]
    | ('A', '2') -> [['^'; '<']; ['<'; '^']]
    | ('A', '0') -> [['<']]
    | ('A', '9') -> [['^'; '^'; '^']]
    | ('A', '6') -> [['^'; '^']]
    | ('A', '3') -> [['^']]
    | ('A', 'A') -> []
    | _ -> 
      Format.printf "%c => %c DNE\n" fromk tok;
      raise Not_found

let dir_distance (fromk : directional_key) (tok : directional_key) 
  : directional_key list list =
  match (fromk, tok) with
  | ('<', '<') -> []
  | ('<', '^') -> [['>'; '^']]
  | ('<', 'v') -> [['>']]
  | ('<', 'A') -> [ 
  (* ['>'; '^'; '>']; 2 switches vs one switch *)
     ['>'; '>'; '^']
    ]
  | ('<', '>') -> [['>'; '>']]
  | ('>', '<') -> [['<'; '<']]
  | ('>', '^') -> [['^'; '<']; ['<'; '^']]
  | ('>', 'v') -> [['<']]
  | ('>', 'A') -> [['^']]
  | ('>', '>') -> []
  | ('^', '<') -> [['v'; '<']]
  | ('^', '^') -> []
  | ('^', 'v') -> [['v']]
  | ('^', 'A') -> [['>']]
  | ('^', '>') -> [['v'; '>']; ['>'; 'v']]
  | ('v', '<') -> [['<']]
  | ('v', '^') -> [['^']]
  | ('v', 'v') -> []
  | ('v', 'A') -> [['^'; '>']; ['>'; '^']]
  | ('v', '>') -> [['>']]
  | ('A', '<') -> [
    ['v'; '<'; '<']; 
 (* ['<'; 'v'; '<'] only one switch necessary *)
  ]
  | ('A', '^') -> [['<']]
  | ('A', 'v') -> [['v'; '<']; ['<'; 'v']]
  | ('A', 'A') -> []
  | ('A', '>') -> [['v']]
  | _ ->
    Format.printf "%c => %c DNE\n" fromk tok;
    raise Not_found

module IString = struct
  type t = string
  let equal = (=)
  let hash (s : t) : int =
    String.hash s
end
module StringTbl = Hashtbl.Make(IString)

let directions_tbl : char list list StringTbl.t = StringTbl.create 10000
let rec to_directions' f (start : char) (path : char list) : char list list =
  let str = ((start :: path) |> List.to_seq |> String.of_seq) in
  if StringTbl.mem directions_tbl str then
    StringTbl.find directions_tbl str
  else
  let ans = match path with
  | x :: xs ->
    let branches = f start x |> fun l -> if List.length l = 0 then [[]] else l in
    branches |> List.map (fun branch ->
      xs |> to_directions' f x |> List.map (fun rst -> branch @ ['A'] @ rst)
    )
      |> List.flatten
  | [] -> [[]]
  in
  (*
    Format.printf "%s => " str;
    ans |> List.iter (fun cl -> cl |> List.iter (Format.printf "%c"); Format.printf ", ");
    Format.printf "\n";
  *)
  if StringTbl.length directions_tbl < 1_000_000_000 then
    StringTbl.add directions_tbl str ans;

  ans

let to_directions f (num : char list list) : char list list =
  num
  |> List.map (to_directions' f 'A')
  |> List.flatten

module CharPair = struct
  type t = char * char
  let equal = (=)
  let hash ((c1, c2) : t) =
    Char.hash c1 * 1000001 + Char.hash c2
  let compare ((c11, c12) : t) ((c21, c22) : t) : int =
    if Char.compare c11 c21 <> 0 then Char.compare c11 c21 else Char.compare c12 c22
end
module CharPairMap = Map.Make(CharPair)

let print_seq (s : directional_key list list) : directional_key list list =
  s |> List.iter (fun s -> 
    List.iter (fun s -> Format.printf "%c" s) s;
    Format.printf ", "
  );
  Format.printf "\n";
  Format.print_flush ();
  s

let part_one (codes : string list) : int = 
  let solve (code : string) : directional_key list =
    Format.printf "\nsolving: %s\n" code;
    code 
    |> String.to_seq 
    |> List.of_seq 
    |> fun l -> [l]
    |> to_directions num_distance
    |> fun l -> 
      Format.printf "len: %d\n" (List.length l);
      Format.print_flush ();
      l
    |> to_directions dir_distance
    |> fun l -> 
      Format.printf "len: %d\n" (List.length l);
      Format.print_flush ();
      l
    |> to_directions dir_distance
    |> fun l -> 
      Format.printf "len: %d\n" (List.length l);
      Format.print_flush ();
      l
    |> List.map (fun path -> (List.length path, path))
    |> List.fold_left 
      (fun (n', p') (n, p) -> if n < n' then (n, p) else (n', p'))
      (1000000000, [])
    |> snd
  in
  codes
    |> List.map (fun c -> (c, solve c |> List.length))
    |> List.map (fun (code, l) -> 
      Format.printf "%s: [%d] " code l;
      Format.printf "\n";
      String.sub code 0 (String.length code - 1)
        |> int_of_string
        |> ( * ) l 
    )
    |> List.fold_left (+) 0

let prune_possibilities (possibilities : directional_key list list) 
  : directional_key list list =
  let lengths = possibilities |> List.map List.length in
  let mean = (lengths |> List.fold_left (+) 0 |> float_of_int) 
    /. (List.length lengths |> float_of_int) in
  let std = ((lengths |> List.map (fun l -> (l |> float_of_int) -. mean) 
    |> List.map (fun f -> f *. f) |> List.fold_left (+.) 0.) /. 
      (List.length possibilities |> float_of_int)) |> Float.sqrt in
  let lengths = lengths |> List.map float_of_int in
  let q1 = lengths |> List.filter (fun l -> l < mean -. 1.*.std) in
  let q2 = lengths |> List.filter (fun l -> l < mean && l >= mean -. 1.*.std) in
  let q3 = lengths |> List.filter (fun l -> l < mean +. 1.*.std && l >= mean) in
  let q4 = lengths |> List.filter (fun l -> l >= mean +. 1.*.std) in
  Format.printf "mean: %f, std: %f, tot: %d, q1: %d, q2: %d, q3: %d, q4: %d\n"
    mean std 
    (lengths |> List.length)
    (q1 |> List.length) (q2 |> List.length) (q3 |> List.length) (q4 |> List.length);
  Format.print_flush ();
  possibilities 
    |> List.filter (fun l -> (List.length l) <= (mean |> int_of_float))

let part_two_slow (codes : string list) : int = 
  let rec solve (ndir : int) (code : directional_key list list) : directional_key list list =
    if ndir = 0 then
      code
    else
    code 
    |> to_directions dir_distance
    |> fun l -> 
      Format.printf "len: %d\n" (List.length l);
      Format.print_flush ();
      l
    |> prune_possibilities
    |> solve (ndir - 1)
  in
  codes
    |> List.map (fun c -> 
      Format.printf "\nsolving: %s\n" c;
      let numerical = c
      |> String.to_seq 
      |> List.of_seq 
      |> fun l -> [l]
      |> to_directions num_distance
      |> fun l -> 
        Format.printf "len: %d\n" (List.length l);
        Format.print_flush ();
        l in
      let dirs = solve 3 numerical in
      (c, dirs
      |> List.map (fun path -> (List.length path, path))
      |> List.fold_left 
        (fun (n', p') (n, p) -> if n < n' then (n, p) else (n', p'))
        (1000000000, [])
      |> snd)
    )
    |> List.map (fun (code, l) -> 
      Format.printf "%s: [%d] " code (List.length l);
      l |> List.iter @@ Format.printf "%c";
      Format.printf "\n";
      String.sub code 0 (String.length code - 1)
        |> int_of_string
        |> ( * ) (List.length l)
    )
    |> List.fold_left (+) 0

let directions_tbl : char list Seq.t StringTbl.t = StringTbl.create 10000
let rec to_directions' f (start : char) (path : char list) : char list Seq.t =
  let s = (start :: path) |> List.to_seq |> String.of_seq in
  if StringTbl.mem directions_tbl s then StringTbl.find directions_tbl s else
  let ans = match path with
    | x :: xs ->
      let branches = f start x |> fun l -> if List.length l = 0 then [[]] else l in
      branches 
      |> List.to_seq
      |> Seq.map (fun branch ->
        xs |> to_directions' f x |> Seq.map (fun rst -> branch @ ['A'] @ rst)
      )
      |> Seq.flat_map Fun.id
    | [] -> List.to_seq [[]]
  in
  if StringTbl.length directions_tbl < 1_000 then
    StringTbl.add directions_tbl s ans;
  ans

let to_directions f (num : string) : string Seq.t =
  num
  |> String.to_seq
  |> List.of_seq
  |> to_directions' f 'A'
  |> Seq.map List.to_seq
  |> Seq.map String.of_seq

module IntTbl = Hashtbl.Make(Int)
type cache_entry = {
  transitions : int;
  length : int
}
type tbl = cache_entry IntTbl.t

let m = ref 0

let count_transitions (code : string) : int =
  let rec help (code : char list) : int =
    match code with
    | c1 :: c2 :: rst when c1 <> c2 -> 1 + help (c2 :: rst)
    | _ :: rst -> help rst
    | [] -> 0
  in
  code |> String.to_seq |> List.of_seq |> help

let part_two' (codes : string list) : int = 
  (* suppose we have a map of int to code length... 
     if we only run solve (ndir - 1) ... if String.length code <= cache[ndir]
     Lets also cache the # of transitions in a code. If 
      cache[ndir].
     *)
  let rec solve ?(solve_cache : tbl = IntTbl.create 1000) (target : int)
    (ndir : int) (code : string) : int =
    let n = String.length code in
    let ntransitions = count_transitions code in
    if 
       not (IntTbl.mem solve_cache ndir)
       || not (
         (IntTbl.find solve_cache ndir).length < n ||
         (IntTbl.find solve_cache ndir).transitions < ntransitions
       )
    then
      IntTbl.add solve_cache ndir {transitions = ntransitions; length = n};
    if ndir = target then begin
      if !m mod 10_000 = 0 then begin
        Format.printf "."; 
        Format.print_flush ()
      end;
      m := !m + 1;
      n
    end else if 
       IntTbl.mem solve_cache ndir && (
         (IntTbl.find solve_cache ndir).length < n ||
         (IntTbl.find solve_cache ndir).transitions < ntransitions
       )
    then begin
      if !m mod 10_000 = 0 then begin
        Format.printf "ignore: %d => %d ; %d\n" ndir ntransitions n;
        Format.print_flush ();
      end;
      m := !m + 1;
      Int.max_int
    end else begin
      if !m mod 100 = 0 then begin
        Format.printf "solve %d (%d)\n" ndir (code |> String.length);
        Format.printf "saving: %d => transitions=%d, length=%d\n" ndir ntransitions n;
        Format.print_flush ();
      end;
      m := !m + 1;
      let directions = code
      |> to_directions dir_distance
      in
      directions
      |> Seq.map (solve ~solve_cache target (ndir + 1))
      |> Seq.fold_left Int.min Int.max_int
    end
  in
  let solve_code (code : string) : unit =
    let ans = code
    |> to_directions num_distance
    |> Seq.map (fun code ->
      (* Iterative deepening! *)
      let cache = IntTbl.create 1000 in
      let _ = solve ~solve_cache:cache 1 0 code in
      let _ = solve ~solve_cache:cache 2 0 code in
      let _ = solve ~solve_cache:cache 3 0 code in
      solve ~solve_cache:cache 4 0 code
    )
    |> Seq.fold_left Int.min Int.max_int in
    Format.printf "code: %s ; %d\n" code ans
  in
  codes |> List.iter solve_code;
  0

type solve_args = {
  target: int;
  ndir: int;
  code: string;
}
module Isolve_args = struct
  type t = solve_args
  let equal (a : t) (b : t) : bool = a = b
  let hash (a : t) = Int.hash a.target + 31 * Int.hash a.ndir + 1001 * String.hash a.code
end
module Tbl = Hashtbl.Make(Isolve_args)
type solve_cache = int Tbl.t

let part_two (codes : string list) : int = 
  (* suppose we have a map of int to code length... 
     if we only run solve (ndir - 1) ... if String.length code <= cache[ndir]
     Lets also cache the # of transitions in a code. If 
      cache[ndir].
     *)
  let rec split_by_A ?(acc = "") (code : string) : string list =
    let n = String.length code in
    if n = 0 && acc <> "" then [acc] else
    if n = 0 then [] else
    if String.get code 0 = 'A' then (acc ^ "A") :: split_by_A (String.sub code 1 (n-1)) else 
    split_by_A ~acc:(acc ^ (String.get code 0 |> String.make 1)) (String.sub code 1 (n-1))
  in

  let rec solve ?(solve_cache = Tbl.create 1000)
    (target : int) (ndir : int) (code : string) : int =
    (* Format.printf "%d %d %s\n" target ndir code;
    Format.print_flush (); *)
    if Tbl.mem solve_cache {target ; ndir ; code} then
      Tbl.find solve_cache {target ; ndir ; code}
    else if target = ndir then
      String.length code 
    else
      let ans = code
        |> split_by_A
        |> List.map (to_directions dir_distance)
        |> List.map (Seq.map (solve ~solve_cache target (ndir+1)))
        |> List.map (Seq.fold_left Int.min Int.max_int)
        |> List.fold_left (+) 0 in
      Tbl.add solve_cache {target ; ndir ; code} ans;
      ans
  in
  let solve_code (code : string) : int =
    code
    |> fun s -> (s, 
      to_directions num_distance s
      |> Seq.map (solve 25 0)
      |> Seq.fold_left Int.min Int.max_int
      )
    |> fun (s, l) -> 
      Format.printf "%s, %d\n" s l;
      Format.print_flush ();
      (String.sub s 0 (String.length s - 1) |> int_of_string) * l
  in
  codes |> List.map solve_code |> List.fold_left (+) 0

let parse_file (file_content : string) : string list =
  file_content |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s <> 0)
(* split by whitespace
   |> List.map (Str.split (Str.regexp "[ \t]+"))
   |> List.map (List.filter (fun s -> not @@ String.equal "" s))
*)
