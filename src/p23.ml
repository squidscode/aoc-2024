(* 
 *  Solves Advent of Code Problem 23:
 *  https://adventofcode.com/2024/day/23
 *)

module Node = struct 
  type t = (char * char)
  let compare ((c1, c2) : t) ((c1', c2') : t) : int =
    let cmp = Char.compare c1 c1' in
    if cmp <> 0 then cmp else Char.compare c2 c2'
  let equal (a : t) (b : t) = a = b
end

module NodeMap = Map.Make(Node)

(** all_subgraphs runs in O(alphabet^n) time. **)
let rec all_subgraphs (n : int) (alphabet : Node.t list) : Node.t list Seq.t =
  if n = 1 then 
    alphabet |> List.to_seq |> Seq.map (fun a -> [a])
  else
  alphabet
  (* lazily iterate over the alphabet *)
  |> List.to_seq
  |> Seq.map (fun a -> 
    let a_index = List.find_index ((=) a) alphabet |> Option.get in
    let woa = alphabet |> List.to_seq |> Seq.drop (a_index + 1) |> List.of_seq in
    all_subgraphs (n-1) woa |> Seq.map (fun nl -> a :: nl))
  |> Seq.flat_map Fun.id

let rec check_clique (subgraph : Node.t list) (graph : Node.t list NodeMap.t) : bool =
  match subgraph with
  | [] -> true
  | a :: rst -> 
    NodeMap.mem a graph &&
    rst |> List.map (fun (node : Node.t) -> 
      let (adj : Node.t list) = NodeMap.find a graph in
      (* for all n in rst : a -> n exists *)
      List.mem node adj
    ) 
    |> List.fold_left (&&) true
    && check_clique rst graph
;;

let part_one (graph : Node.t list NodeMap.t) : int = 
  graph 
  |> NodeMap.to_list 
  |> List.map fst
  |> List.sort_uniq Node.compare
  |> (fun alphabet -> Format.printf "alphabet sz = %d\n" (List.length alphabet); alphabet)
  |> all_subgraphs 3
  |> Seq.filter (fun l -> 
    match l with
    | [(a1, _) ; (b1, _) ; (c1, _)] -> 
      (a1 = 't') || (b1 = 't') || (c1 = 't')
    | _ -> raise Not_found
  )
  |> (fun g -> Format.printf "n subgraphs : %d\n" (g |> List.of_seq |> List.length); g)
  |> Seq.filter (fun g -> check_clique g graph)
  |> List.of_seq
  |> List.map (fun l -> 
    l |> List.iter (fun (a,b) -> Format.printf "%c%c, " a b);
    Format.printf "\n";
    l
  )
  |> List.filter (fun l -> 
    match l with
    | [(a1, _) ; (b1, _) ; (c1, _)] -> 
      (a1 = 't') || (b1 = 't') || (c1 = 't')
    | _ -> raise Not_found
  )
  |> List.length

(*
 algorithm BronKerbosch1(R, P, X) is
    if P and X are both empty then
        report R as a maximal clique
    for each vertex v in P do
        BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
        P := P \ {v}
        X := X ⋃ {v} 
*)
module NodeSet = Set.Make(Node)

let print_node_set (s : NodeSet.t) : unit =
  Format.printf "{";
  s |> NodeSet.iter (fun (c1,c2) -> Format.printf "%c%c," c1 c2);
  Format.printf "}";
  Format.print_flush ()

let part_two (graph : Node.t list NodeMap.t) : int = 
  let rec bron_kerbosch (r : NodeSet.t) (p : NodeSet.t) (x : NodeSet.t) : NodeSet.t list =
    Format.printf "bron_kerbosch ";
    print_node_set r;
    Format.printf " ";
    print_node_set p;
    Format.printf " ";
    print_node_set x;
    Format.printf "\n";
    if NodeSet.cardinal p = 0 && NodeSet.cardinal x = 0 then
      [r]
    else
    p
    |> (fun xxx -> NodeSet.fold (fun v (r, p, x, l) ->
      let r' = NodeSet.add v r in
      let nx = NodeMap.find v graph |> NodeSet.of_list in
      let p' = NodeSet.inter p nx in
      let x' = NodeSet.inter x nx in
      let pwov = NodeSet.remove v p in
      let xwv = NodeSet.add v x in
      (r, pwov, xwv, (bron_kerbosch r' p' x') @ l)
    ) xxx (r, p, x, []))
    |> fun (_,_,_,l) -> l
  in
  let sets = bron_kerbosch 
    NodeSet.empty 
    (graph |> NodeMap.to_list |> List.map fst |> NodeSet.of_list)
    NodeSet.empty
  in
  let ans = sets
  |> List.map NodeSet.cardinal
  |> List.fold_left Int.max Int.min_int
  in
  Format.printf "sets:\n";
  sets 
    |> List.filter (fun s -> NodeSet.cardinal s = ans)
    |> List.iter (fun s -> 
      print_node_set s;
      Format.printf "\n";
    );
  ans

let edge_re = "(\\w{2})-(\\w{2})" |> Re.Perl.re |> Re.Perl.compile

let s_to_node s =
  (s |> Fun.flip String.get 0, s |> Fun.flip String.get 1)

let parse_file (file_content : string) : Node.t list NodeMap.t =
  file_content |> String.split_on_char '\n'
  |> List.map (Re.all edge_re)
  |> List.flatten
  |> List.map (fun g -> (Re.Group.get g 1, Re.Group.get g 2))
  |> List.map (fun (a, b) -> (a |> s_to_node, b |> s_to_node))
  |> List.fold_left (fun l (a,b) -> (a,b) :: (b,a) :: l) []
  |> List.fold_left (fun assoc (a,b) -> 
    match List.assoc_opt a assoc with
    | None -> (a,[b]) :: assoc
    | Some(b') -> (a, b :: b') :: (List.remove_assoc a assoc)) []
  |> NodeMap.of_list
