
module INT = Isa.IntSet
module T = Isa.IntOps
open INT

type intermediate_representation =
  | Implicit of string
  | Explicit of (string * T.t)

type result =
  | Inst of instruction
  | Unparsed of intermediate_representation

let (>>=) (arg : result) fn =
  match arg with
  | Unparsed _ as x -> fn x
  | Inst _ as success -> success

exception ParseFailure of string
let parse_fail str = raise (ParseFailure str)
let list_to_string lst = String.concat "," lst

let opcode_and_operands = function
  | [ opcode ; operand ]
    -> Explicit (opcode, T.of_string operand)
  | [ opcode ]
    -> Implicit opcode
  | lst -> parse_fail (list_to_string lst)

let unwrap_monad fn = function
  | Unparsed x -> fn x
  | Inst _ as i -> i

let try_parse fn ops =
  try Inst (fn ops)
  with _ -> Unparsed ops

let parse_memory ops =
  let parse = function
    | Explicit ("iload",  num) -> Mem (LOAD num)
    | Explicit ("istore", num) -> Mem (STORE num)
    | _ -> parse_fail "parse_memory"
  in try_parse parse ops


let parse_variable ops =
  let parse = function
    | Explicit ("iconst",num) -> Var (CONST num)
    | Explicit ("bipush",num | "sipush", num) -> Var (PUSH num)
    | _ -> parse_fail "parse_variable"
  in try_parse parse ops

let parse_explicit ops =
  ops
  |> unwrap_monad parse_memory
  |> (unwrap_monad parse_variable)

let parse_alu op =
  let parse = function
    | Implicit "iadd" -> Alu ADD
    | Implicit "isub" -> Alu SUB
    | Implicit "imul" -> Alu MUL
    | Implicit "idiv" -> Alu DIV
    | _ -> parse_fail "parse_alu"
  in try_parse parse op

let parse_implicit ops =
  ops
  |> unwrap_monad parse_alu

let parse (line : string list) =
  let tokens = opcode_and_operands line in
  let ir = Unparsed tokens in
  match ir with
  | Unparsed (Explicit _) as expr ->
    parse_explicit expr
  | Unparsed (Implicit _) as expr ->
    parse_implicit expr
  | other -> other

let parse (text : string) =
  text
  |> Stringtrim.get_instructions
  |> List.map parse
