exception ArgumentError of string

let () =
  match Array.length Sys.argv with
  | 3 ->
      let s =
        In_channel.with_open_text (Array.get Sys.argv 2) In_channel.input_all
      in
      (match Array.get Sys.argv 1 with
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
      | "8.1" -> s |> P8.parse_file |> P8.part_one
      | "8.2" -> s |> P8.parse_file |> P8.part_two
      | "9.1" -> s |> P9.parse_file |> P9.part_one
      | "9.2" -> s |> P9.parse_file |> P9.part_two
      | "10.1" -> s |> P10.parse_file |> P10.part_one
      | "10.2" -> s |> P10.parse_file |> P10.part_two
      | "11.1" -> s |> P11.parse_file |> P11.part_one
      | "11.2" -> s |> P11.parse_file |> P11.part_two
      | "12.1" -> s |> P12.parse_file |> P12.part_one
      | "12.2" -> s |> P12.parse_file |> P12.part_two
      | "13.1" -> s |> P13.parse_file |> P13.part_one
      | "13.2" -> s |> P13.parse_file |> P13.part_two
      | "14.1" -> s |> P14.parse_file |> P14.part_one
      | "14.2" -> s |> P14.parse_file |> P14.part_two
      | "15.1" -> s |> P15.parse_file |> P15.part_one
      | "15.2" -> s |> P15.parse_file |> P15.part_two
      | "16.1" -> s |> P16.parse_file |> P16.part_one
      | "16.2" -> s |> P16.parse_file |> P16.part_two
      | "17.1" -> s |> P17.parse_file |> P17.part_one
      | "17.2" -> s |> P17.parse_file |> P17.part_two
      | "18.1" -> s |> P18.parse_file |> P18.part_one
      | "18.2" -> s |> P18.parse_file |> P18.part_two
      | "19.1" -> s |> P19.parse_file |> P19.part_one
      | "19.2" -> s |> P19.parse_file |> P19.part_two
      | "20.1" -> s |> P20.parse_file |> P20.part_one
      | "20.2" -> s |> P20.parse_file |> P20.part_two
      | "21.1" -> s |> P21.parse_file |> P21.part_one
      | "21.2" -> s |> P21.parse_file |> P21.part_two
      | "22.1" -> s |> P22.parse_file |> P22.part_one
      | "22.2" -> s |> P22.parse_file |> P22.part_two
      | "23.1" -> s |> P23.parse_file |> P23.part_one
      | "23.2" -> s |> P23.parse_file |> P23.part_two
      | "24.1" -> s |> P24.parse_file |> P24.part_one
      | "24.2" -> s |> P24.parse_file |> P24.part_two
      | "25.1" -> s |> P25.parse_file |> P25.part_one
      | "25.2" -> s |> P25.parse_file |> P25.part_two
      (* TEMPLATE.SH WILL INSERT HERE *)
      | _ -> raise (ArgumentError "Bad first argument"))
      |> Format.printf "answer: %d\n"
  | _ ->
      Printf.eprintf "Invalid argument length\n";
      exit 1
