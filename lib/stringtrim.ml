let clean = List.filter ((<>) "")

let get_lines = String.split_on_char '\n'

let get_words = String.split_on_char ' '

let remove_underscores line =
  let rec handle = function
  | hd :: tl ->
    let hd = hd |> String.split_on_char '_' in
    hd @ (handle tl)
  | [] -> []
  in handle line

let tokenize line =
  line
  |> get_words
  |> clean
  |> remove_underscores
  |> clean

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
