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

  type variable =
    | CONST of T.t
    | PUSH of T.t

  type instruction =
    | Var of variable
    | Mem of memory_access
    | Alu of alu
    | NOP
end

module IntOps : Type = struct
  type t = int
  let of_string = int_of_string
  let to_string = string_of_int
  let c = 'i'
end

module IntSet = Instructions(IntOps : Type)
