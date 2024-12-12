exception ArgumentError of string

let () =
  match (Array.length Sys.argv) with
  | 3 ->
    let s = In_channel.with_open_text (Array.get Sys.argv 2) In_channel.input_all in 
    begin match (Array.get Sys.argv 1) with 
    (*  Add run options here  *)
    (*  TODO: is there a way to avoid P# duplication? Maybe a macro? *)
    (*  https://ocaml-ppx.github.io/ppxlib/ppxlib/index.html  *)
    | "1.1" -> s |> P1.parse_file |> P1.part_one
    | "1.2" -> s |> P1.parse_file |> P1.part_two

    | "2.1" -> s |> P2.parse_file |> P2.part_one
    | "2.2" -> s |> P2.parse_file |> P2.part_two

    | "3.1" -> s |> P3.parse_file |> P3.part_one
    | "3.2" -> s |> P3.parse_file_two |> P3.part_two

    | "4.1" -> s |> P4.parse_file |> P4.part_one
    | "4.2" -> s |> P4.parse_file |> P4.part_two

    | "5.1" -> s |> P5.parse_file |> P5.part_one
    | "5.2" -> s |> P5.parse_file |> P5.part_two

    | "6.1" -> s |> P6.parse_file |> P6.part_one
    | "6.2" -> s |> P6.parse_file |> P6.part_two

    | "7.1" -> s |> P7.parse_file |> P7.part_one
    | "7.2" -> s |> P7.parse_file |> P7.part_two

    | _ -> raise (ArgumentError "Bad first argument")
    end |> Format.printf "answer: %d\n"
  | _ -> Printf.eprintf "Invalid argument length\n"; exit 1
;;
