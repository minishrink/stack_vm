
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
  try
    code
    |> INT.parse_to_string
    |> List.iter print_endline
  with e -> raise e

let _ =
  test_parsing Example.fact_ex;
  print_endline "Successful parsing!"

