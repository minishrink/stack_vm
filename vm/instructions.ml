module OPCODES (V : Machine.Value) = struct

  type var_op =
    | BIPUSH of V.t
    | SIPUSH of V.t
    | CONST of V.t

  type mem_op =
    | LOAD of V.m
    | STORE of V.m
    | INC of V.m * V.t

  type binary_op =
    | ADD
    | SUB
    | MUL
    | DIV
    | REM
    | AND
    | OR
    | XOR
    | LSHIFT
    | RSHIFT
(*  | U_LSHIFT
    | U_RSHIFT
 *)

  type unary_op =
    | NEG

  type component =
    | Alu
    | Mem
    | Stk
    | Var

  type opcode

  type instruction =
    { opcode    : opcode
    ; component : component
    ; cycles    : int
    }
end
