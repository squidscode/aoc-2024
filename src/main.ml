exception ArgumentError of string

let () =
  match (Array.length Sys.argv) with
  | 3 ->
    let s = In_channel.with_open_text (Array.get Sys.argv 2) In_channel.input_all in 
    begin match (Array.get Sys.argv 1) with 
    (*  Add run options here  *)
    (*  TODO: is there a way to avoid P# duplication?  *)
    | "1.1" -> s |> P1.parse_file |> P1.part_one
    | "1.2" -> s |> P1.parse_file |> P1.part_two

    | "2.1" -> s |> P2.parse_file |> P2.part_one
    | "2.2" -> s |> P2.parse_file |> P2.part_two

    | "3.1" -> s |> P3.parse_file |> P3.part_one
    | "3.2" -> s |> P3.parse_file_two |> P3.part_two

    | _ -> raise (ArgumentError "Bad first argument")
    end |> Format.printf "answer: %d\n"
  | _ -> Printf.eprintf "Invalid argument length\n"; 
;;
