let clean = List.filter ((<>) "")

let get_lines = String.split_on_char '\n'

let get_words = String.split_on_char ' '

let remove_underscores line =
  match line with
  | hd :: tl ->
    let hd = hd |> String.split_on_char '_' in
    hd @ tl
  | [] -> []

let get_instructions text =
  text
  |> get_lines
  |> List.map
    (fun line -> line |> get_words |> clean)
  |> List.filter
    (function | [] -> false | _ -> true)
  |> List.map remove_underscores

let snippet = "
  iconst_9
  iload_5
  istore_2
  add
  bipush
  nop

  newarray"
