let clean = List.filter ((<>) "")

let get_lines = String.split_on_char '\n'

let get_words = String.split_on_char ' '

let remove char line =
  let rec handle = function
  | hd :: tl ->
    let hd = hd |> String.split_on_char char in
    hd @ (handle tl)
  | [] -> []
  in handle line

let trim line =
  line
  |> remove '_'
  |> remove ','

let handle_branches str =
  if String.length str < 3 then str
  else begin
    if String.sub str 0 3 = "if_"
    then try
      String.sub str 3 (String.length str - 3)
    with _ -> str
    else str
end

let tokenize line =
  line
  |> get_words
  |> clean
  |> List.map handle_branches
  |> trim
  |> List.filter (fun str -> not (List.mem str [","; ":";".";""]))

let get_instructions text =
  text
  |> get_lines
  |> List.map tokenize
  |> List.filter (function | [] -> false | _ -> true)
  |> List.map List.tl

let snippet = "
  iconst_9
  iload_5
  istore_2
  add
  bipush
  nop

  newarray"
