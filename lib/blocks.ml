module ISA = Int_parser.INT
module PARSE = Int_parser

type basic_block = ISA.instruction list

let build_block instructions =
  let rec build dfg = function
    | (ISA.(Flow _) as f) :: tl -> (List.rev (f::dfg)) :: (build [] tl)
    | hd :: tl -> build (hd :: dfg) tl
    | [] -> []
  in
  build [] instructions

(* Unwrap monad to give instructions *)
let unwrap = PARSE.(function
    | Instruction x -> x
    | Unparsed _ -> ISA.NOP
  )

let blocks_of (instructions: PARSE.result list) =
  instructions
  |> List.map unwrap
  |> build_block

let to_string basic_block =
  basic_block
  |> List.map ISA.Printer.to_string
  |> String.concat "\n"
  |> (Printf.sprintf
        "\n--BEGIN BLOCK--\n%s\n--END BLOCK--\n"
     )

let parse_to_blockstring text =
  text
|> PARSE.parse
|> blocks_of
|> List.map to_string
|> String.concat "\n"
