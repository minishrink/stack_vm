module type Type = sig
  type t
  val c : char
  val of_string : string -> t
  val to_string : t -> string
end

module Instructions (T : Type) = struct
  include T
  exception ParseError of string
  let parse_error instruction =
    let err = (String.concat " " instruction) in
    raise (ParseError err)

  type memory_access =
    | LOAD of T.t
    | STORE of T.t

  type alu =
    | ADD
    | SUB
    | MUL
    | DIV
    | IINC of (T.t * T.t)

  type variable =
    | CONST of T.t
    | PUSH of T.t

  (* T.t should just be hardwired as int tbh *)
  type control_flow =
    | RETURN
    | GOTO
    | IF_GT of T.t
    | IF_GE of T.t

  type instruction =
    | Var of variable
    | Mem of memory_access
    | Alu of alu
    | Flow of control_flow
    | NOP

  module Printer = struct

    let concat str x = str ^ " " ^ (T.to_string x)

    let flow_string = function
      | RETURN ->"RETURN"
      | GOTO ->"GOTO"
      | IF_GT l -> concat "IF_GT" l
      | IF_GE l -> concat "IF_GE" l

    let mem_string = function
      | LOAD n -> concat "LOAD" n
      | STORE n -> concat "STORE" n

    let var_string = function
      | CONST n -> concat "CONST" n
      | PUSH n -> concat "PUSH" n

    let alu_string = function
      | ADD -> "ADD"
      | SUB -> "SUB"
      | MUL -> "MUL"
      | DIV -> "DIV"
      | IINC (s,t) -> concat (concat "IINC" s) t

    let to_string = function
      | Mem i -> mem_string i
      | Var i -> var_string i
      | Alu i -> alu_string i
      | NOP -> "NOP"
      | Flow i -> flow_string i

    let to_string i = try to_string i with _ -> "FUCK"

  end
end

module IntOps : Type = struct
  type t = int
  let of_string x = try int_of_string x with _ -> 0
  let to_string = string_of_int
  let c = 'i'
end

module IntSet = Instructions(IntOps : Type)
