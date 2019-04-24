
module INT = Int_parser

let print_code block =
  block
  |> List.map (String.concat " ")
  |> String.concat "\n"
  |> print_endline

let demo code =
  code
  |> Stringtrim.get_instructions
  |> print_code

let test line =
  let lst = Stringtrim.tokenize line in
  lst
  |> String.concat " "
  |> print_endline

let test_parsing code =
  print_endline "\n===<BEGIN BLOCK>===";
  try
    code
    |> INT.parse_to_string
    |> List.iter (fun str -> ("  " ^ str) |> print_endline);
  print_endline "===<END BLOCK>===\n"
  with e -> raise e

let _ =
  List.iter test_parsing Example.([fact_ex; list_code]);
  print_endline "Successful parsing!"

