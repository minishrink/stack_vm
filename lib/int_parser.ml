
module INT = Isa.IntSet
module T = Isa.IntOps
open INT

type intermediate_representation =
  | Implicit of string
  | Explicit of (string * T.t)
  | CISC of (string * T.t * T.t)
  | Object of (string * string)

let string_of_ir = function
  | Implicit s -> s
  | Explicit (s,t) ->
    Printf.sprintf "%s %s" s (T.to_string t)
  | CISC (s,t,u) ->
    Printf.sprintf "%s %s %s" s (T.to_string t) (T.to_string u)
  | Object (dec,t) ->
    Printf.sprintf "%s %s" dec t

type result =
  | Instruction of instruction
  | Unparsed of intermediate_representation

let (>>=) (arg : result) fn =
  match arg with
  | Unparsed _ as x -> fn x
  | Instruction _ as success -> success

exception ParseFailure of string
let parse_fail str = raise (ParseFailure str)
let list_to_string lst = String.concat "," lst

let object_decs = ["newarray"; "aload"; "astore"; "iaload"; "iastore"]

let opcode_and_operands = function
  | [ opcode ; operand ] when List.mem opcode object_decs
    -> Object (opcode, operand)
  | [ opcode ; operand ]
    -> Explicit (opcode, T.of_string operand)
  | [ opcode ]
    -> Implicit opcode
  | [ one ; two ;three ] when one="iinc"
    -> CISC (one, T.of_string two, T.of_string three)
  | lst -> parse_fail (list_to_string lst)

let unwrap_monad fn = function
  | Unparsed x -> fn x
  | Instruction _ as i -> i

let try_parse fn ops =
  try Instruction (fn ops)
  with _ -> Unparsed ops

let parse_memory ops =
  let parse = function
    | Explicit ("iload",  num) -> Mem (LOAD num)
    | Explicit ("istore", num) -> Mem (STORE num)
    | Object ("newarray", t) -> Mem (ARRAY t)
    | _ -> parse_fail "parse_memory"
  in try_parse parse ops


let parse_variable ops =
  let parse = function
    | Explicit ("iconst",num) -> Var (CONST num)
    | Explicit ("bipush",num | "sipush", num) -> Var (PUSH num)
    | _ -> parse_fail "parse_variable"
  in try_parse parse ops

let parse_manip ops =
  let parse = function
    | Implicit "dup" -> Manip (DUP)
    | Implicit "swap" -> Manip (SWAP)
    | _ -> parse_fail "parse_manip"
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
    | CISC ("iinc", mem, incr) -> Alu (IINC (mem, incr))
    | _ -> parse_fail "parse_alu"
  in try_parse parse op

let parse_flow op =
  let parse = function
    | Implicit ("ireturn" | "return") -> Flow RETURN
    | Implicit ("goto") -> Flow GOTO
    | Explicit ("icmpgt",l) -> Flow (IF_GT l)
    | Explicit ("icmpge",l) -> Flow (IF_GE l)
    | _ -> parse_fail "parse_flow"
  in try_parse parse op

let implicit_parsers =
  [ parse_alu
  ; parse_flow
  ; parse_manip
  ]

let parse_implicit ops =
  List.fold_left
    (fun monad parse_fn -> unwrap_monad parse_fn monad)
    ops
    implicit_parsers

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

let print = function
  | Instruction i -> Printer.to_string i
  | Unparsed i -> string_of_ir i

let parse_to_string text =
  text
  |> parse
  |> List.map print
