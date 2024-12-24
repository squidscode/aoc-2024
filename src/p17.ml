(*
 *  Solves Advent of Code Problem 17:
 *  https://adventofcode.com/2024/day/17
 *)

open Format

type literal = Zero | One | Two | Three
type combo = Literal of literal | RegA | RegB | RegC

type instruction =
  | Adv of combo (* A := A / 2^combo *)
  | Bxl of int (* B := B ^ literal *)
  | Bst of combo (* B := combo % 8 *)
  | Jnz of int (* IP := literal *)
  | Bxc (* B := B ^ C *)
  | Out of combo (* stdout <- combo *)
  | Bdv of combo (* B := A / 2^combo *)
  | Cdv of combo (* C := A / 2^combo *)

type reg = int64
type program = { a : reg; b : reg; c : reg; program : int list; ip : int }

(* an io option *)
type io = string option

let to_int (literal : literal) : int =
  match literal with Zero -> 0 | One -> 1 | Two -> 2 | Three -> 3

let to_literal (literal : int) : literal =
  match literal with
  | 0 -> Zero
  | 1 -> One
  | 2 -> Two
  | 3 -> Three
  | _ -> raise Not_found

let to_combo (combo : int) : combo =
  match combo with
  | 0 | 1 | 2 | 3 -> Literal (to_literal combo)
  | 4 -> RegA
  | 5 -> RegB
  | 6 -> RegC
  | _ -> raise Not_found

let int_of_combo (combo : combo) (computer : program) : int64 =
  match combo with
  | Literal l -> Int64.of_int (to_int l)
  | RegA -> computer.a
  | RegB -> computer.b
  | RegC -> computer.c

let to_instruction (op : int) (combo : int) : instruction =
  match op with
  | 0 -> Adv (to_combo combo)
  | 1 -> Bxl combo
  | 2 -> Bst (to_combo combo)
  | 3 -> Jnz combo
  | 4 -> Bxc
  | 5 -> Out (to_combo combo)
  | 6 -> Bdv (to_combo combo)
  | 7 -> Cdv (to_combo combo)
  | _ -> raise Not_found

(** Given the current state of the computer, take a step of execution
    this involves, reading the pc, getting the instruction at the pc, 
    executing the instruction, returning the output and the new computer 
    state if it exists. *)
let step (program : program) : (program * io) option =
  assert (List.length program.program mod 2 = 0);
  if List.length program.program - 2 < program.ip then
    None
  else
    let op = List.nth program.program program.ip in
    let combo = List.nth program.program (program.ip + 1) in
    let instruction = to_instruction op combo in
    let c, io =
      match instruction with
      | Adv combo ->
          ( {
              program with
              a =
                Int64.shift_right program.a
                  (Int64.to_int (int_of_combo combo program));
            },
            None )
      | Bxl literal ->
          ( { program with b = Int64.logxor program.b (literal |> Int64.of_int) },
            None )
      | Bst combo ->
          ( {
              program with
              b = Int64.logand (int_of_combo combo program) (Int64.of_int 0b111);
            },
            None )
      | Jnz literal ->
          (* Set to literal - 2 ==> (literal - 2) + 2 == literal *)
          if program.a = Int64.zero then
            (program, None)
          else
            ({ program with ip = literal - 2 }, None)
      | Bxc -> ({ program with b = Int64.logxor program.b program.c }, None)
      | Out combo ->
          ( program,
            Some
              (Int64.logand (int_of_combo combo program) (Int64.of_int 0b111)
              |> Int64.to_string) )
      | Bdv combo ->
          ( {
              program with
              b =
                Int64.shift_right program.a
                  (Int64.to_int (int_of_combo combo program));
            },
            None )
      | Cdv combo ->
          ( {
              program with
              c =
                Int64.shift_right program.a
                  (Int64.to_int (int_of_combo combo program));
            },
            None )
    in
    Some ({ c with ip = c.ip + 2 }, io)

let combo_to_string (combo : combo) : string =
  match combo with
  | Literal literal -> (to_int literal |> string_of_int) ^ " "
  | RegA -> "%a"
  | RegB -> "%b"
  | RegC -> "%c"

let print_instruction (ins : instruction) : unit =
  match ins with
  | Adv combo -> printf "adv %s" (combo_to_string combo)
  | Bxl int -> printf "bxl %d " int
  | Bst combo -> printf "bst %s" (combo_to_string combo)
  | Jnz int -> printf "jnz %d " int
  | Bxc -> printf "bxc   "
  | Out combo -> printf "out %s" (combo_to_string combo)
  | Bdv combo -> printf "bdv %s" (combo_to_string combo)
  | Cdv combo -> printf "cdv %s" (combo_to_string combo)

let rec int_to_octal (i : int) : string =
  match i with
  | n when 0 <= n && n <= 7 -> string_of_int n
  | n -> int_to_octal (n / 8) ^ string_of_int (n mod 8)

let print_program (c : program) : unit =
  if c.ip < List.length c.program - 1 then (
    let op = List.nth c.program c.ip in
    let combo = List.nth c.program (c.ip + 1) in
    printf "  ";
    to_instruction op combo |> print_instruction;
    printf "\t\t");
  printf "; Computer state: a=%s, b=%s, c=%s, ip=%d\n"
    (Int64.to_int c.a |> int_to_octal)
    (Int64.to_int c.b |> int_to_octal)
    (Int64.to_int c.c |> int_to_octal)
    c.ip

let rec run ?(revio : string list = []) (c : program) : string list =
  (* print_program c;*)
  match step c with
  | Some (c, Some r) -> run ~revio:(r :: revio) c
  | Some (c, None) -> run ~revio c
  | None -> List.rev revio

let part_one (c : program) : int =
  let out = c |> run in
  printf "Output: ";
  out |> List.iter (fun s -> printf "%s," s);
  printf "\n";
  out |> List.fold_left (fun acc r -> acc ^ r) "" |> int_of_string

(** run the computation until the program returns some io, at which point
    return the new program and the output *)
let rec run_until_output (c : program) : (program * string) option =
  (* print_computer c; *)
  match step c with
  | Some (c, Some r) -> Some (c, r)
  | Some (c, None) -> run_until_output c
  | None -> None

let instructions_to_int (instructions : int list) : int =
  instructions |> fun l -> List.fold_right (fun i acc -> (10 * acc) + i) l 0

let output_compare_sz (o1 : int list) (o2 : int list) : int =
  let n = Int.compare (List.length o1) (List.length o2) in
  if n <> 0 then n else 0

let output_compare (o1 : int list) (o2 : int list) : int =
  List.fold_right2
    (fun i1 i2 acc -> if acc <> 0 then acc else Int.compare i1 i2)
    o1 o2 0

let rec binary_searchr (oc : int list -> int list -> int) (program : program)
    (target : int list) (min : int) (max : int) : int =
  if max - min = 1 then
    min
  else
    let mid = (max + min) / 2 in
    let new_prog = { program with a = Int64.of_int mid } in
    let mid_output = run new_prog |> List.map int_of_string in
    printf "[%#d,%#d) , mid = %#d --> " min max mid;
    printf "sz=%d, " (List.length mid_output);
    mid_output |> List.iter (fun i -> printf "%d," i);
    printf "\n";
    let c = oc target mid_output in
    if c = 0 then
      binary_searchr oc program target mid max
    else if c > 0 then
      binary_searchr oc program target mid max
    else
      binary_searchr oc program target min mid

let rec binary_searchl (oc : int list -> int list -> int) (program : program)
    (target : int list) (min : int) (max : int) : int =
  if max - min = 1 then
    min
  else
    let mid = (max + min) / 2 in
    let new_prog = { program with a = Int64.of_int mid } in
    let mid_output = run new_prog |> List.map int_of_string in
    printf "[%#d,%#d) , mid = %#d --> " min max mid;
    printf "sz=%d, " (List.length mid_output);
    mid_output |> List.iter (fun i -> printf "%d," i);
    printf "\n";
    let c = oc target mid_output in
    if c = 0 then
      binary_searchl oc program target min mid
    else if c > 0 then
      binary_searchl oc program target mid max
    else
      binary_searchl oc program target min mid

let rec solve (c : program) (target : int list) (l : int) (r : int) : int =
  if l = r then
    raise Not_found
  else
    printf "A: %s, " (l |> int_to_octal);
  let rl = run { c with a = Int64.of_int l } |> List.map int_of_string in
  printf "output: ";
  rl |> List.iter (fun i -> printf "%d," i);
  printf "\n";
  let rhalf = rl |> List.rev |> List.to_seq |> Seq.take 4 |> List.of_seq in
  let thalf = target |> List.rev |> List.to_seq |> Seq.take 4 |> List.of_seq in
  let rhalf' = rl |> List.rev |> List.to_seq |> Seq.take 8 |> List.of_seq in
  let thalf' = target |> List.rev |> List.to_seq |> Seq.take 8 |> List.of_seq in

  if thalf <> rhalf then
    solve c target (l + 100_000_000) r
  else if thalf' <> rhalf' then
    solve c target (l + 1_000_000) r
  else if target <> rl then
    solve c target (l + 1) r
  else
    l

(** exponential run-up until we go over the target *)
let part_two (c : program) : int =
  printf "Instructions: ";
  printf "sz=%d, " (List.length c.program);
  c.program |> List.iter (fun i -> printf "%d," i);
  printf "\n";
  let min = 0 in
  let max = 1_000_000_000_000_000 in
  let lans = binary_searchl output_compare_sz c c.program min max in
  let rans = binary_searchr output_compare_sz c c.program min max in
  printf "bin search left: %#d\n" lans;
  printf "bin search right: %#d\n" rans;
  printf "Instructions: ";
  printf "sz=%d, " (List.length c.program);
  c.program |> List.iter (fun i -> printf "%d," i);
  printf "\n";
  solve c c.program (lans + 1) rans

let re =
  "Register A: (\\d+)\n\
   Register B: (\\d+)\n\
   Register C: (\\d+)\n\n\
   Program: (.*)\n"

let parse_file (file_content : string) : program =
  file_content |> Re.all (Re.Perl.re re |> Re.compile) |> fun l ->
  assert (List.length l = 1);
  l |> List.hd |> fun g ->
  let a = Int64.of_string @@ Re.Group.get g 1 in
  let b = Int64.of_string @@ Re.Group.get g 2 in
  let c = Int64.of_string @@ Re.Group.get g 3 in
  let instructions =
    Re.Group.get g 4 |> String.split_on_char ',' |> List.map int_of_string
  in
  { a; b; c; program = instructions; ip = 0 }
