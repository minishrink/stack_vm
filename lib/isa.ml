module Instructions = struct
  exception ParseError of string
  let parse_error instruction =
    let err = (String.concat " " instruction) in
    raise (ParseError err)

  type stack_manip =
    | DUP
    | SWAP

  type memory_access =
    | LOAD of int
    | STORE of int
    | ARRAY of string

  type alu =
    | ADD
    | SUB
    | MUL
    | DIV
    | AND
    | OR
    | XOR
    | NEG
    | IINC of (int * int)

  type variable =
    | CONST of int
    | PUSH of int

  (* int should just be hardwired as int tbh *)
  type control_flow =
    | RETURN
    | GOTO
    | IF_GT of int
    | IF_GE of int

  type instruction =
    | Var of variable
    | Mem of memory_access
    | Alu of alu
    | Flow of control_flow
    | Manip of stack_manip
    | NOP

  module Printer = struct

    let concat str x = str ^ " " ^ (string_of_int x)

    let manip_string = function
      | DUP -> "DUP"
      | SWAP -> "SWAP"

    let flow_string = function
      | RETURN ->"RETURN"
      | GOTO ->"GOTO"
      | IF_GT l -> concat "IF_GT" l
      | IF_GE l -> concat "IF_GE" l

    let mem_string = function
      | LOAD n -> concat "LOAD" n
      | STORE n -> concat "STORE" n
      | ARRAY s -> "ARRAY " ^ s

    let var_string = function
      | CONST n -> concat "CONST" n
      | PUSH n -> concat "PUSH" n

    let alu_string = function
      | ADD -> "ADD"
      | SUB -> "SUB"
      | MUL -> "MUL"
      | DIV -> "DIV"
      | AND -> "AND"
      | OR -> "OR"
      | XOR -> "XOR"
      | NEG -> "NEG"
      | IINC (s,t) -> concat (concat "IINC" s) t

    let to_string = function
      | Mem i -> mem_string i
      | Var i -> var_string i
      | Alu i -> alu_string i
      | NOP -> "NOP"
      | Flow i -> flow_string i
      | Manip sm -> manip_string sm

  end
end
