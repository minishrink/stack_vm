
type operandArity =
  | Implicit of string
  (* second string can be a register, a stack value, or a memory value *)
  | Explicit of string * string
  | NOINSTR

(* one line *)
let lex instruction =
  match (String.split_on_char ' ' instruction) with
  | [ instruction ; operand ] -> Explicit (instruction, operand)
  | [ instruction ]           -> Implicit instruction
  | _                         -> NOINSTR

module InstructionParser(A : Machine.Architecture) = struct

  type instruction =
    | NOP
    (* arithmetic *)
    | ADD
    | SUB
    | INC
    | DEC
    (* logical *)
    | AND
    | OR
    | NOT
    | XOR
    | SHR
    | SHL
    (* comparative *)
    | TGT
    | TLT
    | TEQ
    | TSZ
    (* memory *)
    | SSET of A.stack_value
    | SET of A.stack_value
    | LOAD
    | STORE
    (* branch
       | BRANCH
       | BRZERO
       | IBRANCH
       | CALL
       | RETURN
       | STOP *)
    (* manipulation *)
    | DROP
    | DUP
    | SWAP
    | RSD3
    | RSU3
    | TUCK2
    | TUCK3
    | COPY3
    | PUSH of A.register
    | POP of A.register

  let implicit_parse = function
    (* arithmetic *)
    | "ADD" -> ADD
    | "SUB" -> SUB
    | "INC" -> INC
    | "DEC" -> DEC
    (* logical *)
    | "AND" -> AND
    | "OR" -> OR
    | "NOT" -> NOT
    | "XOR" -> XOR
    | "SHR" -> SHR
    | "SHL" -> SHL
    (* comparative *)
    | "TGT" -> TGT
    | "TLT" -> TLT
    | "TEQ" -> TEQ
    | "TSZ" -> TSZ
    (* memory *)
    | "LOAD" -> LOAD
    | "STORE" -> STORE
    (* manipulation *)
    | "DROP" -> DROP
    | "DUP" -> DUP
    | "SWAP" -> SWAP
    | "RSD3" -> RSD3
    | "RSU3" -> RSU3
    | "TUCK2" -> TUCK2
    | "TUCK3" -> TUCK3
    | "COPY3" -> COPY3
    | _ -> NOP

  let explicit_parse op = function
    | "PUSH" -> PUSH (A.to_register op)
    | "POP" -> POP (A.to_register op)
    | "SSET" -> SSET (A.to_stack_value op)
    | "SET" -> SET (A.to_stack_value op)
    | _ -> NOP
end
