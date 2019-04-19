
type t = int
type memory


type 't var_op =
   | BIPUSH of 't
   | SIPUSH of 't
   | CONST of 't

type 'm mem_op =
  | LOAD of 'm
  | STORE of 'm

type binary_op =
  (** pop top
   *  pop next
   *  perform op
   *  push result
   **)
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
 (* | U_LSHIFT
    | U_RSHIFT
 *)

type unary_op =
  | NEG

type component =
  | Alu
  | Mem
  | Stk
  | Var

type ('t, 'm) opcode

type ('t, 'm) instruction =
  { opcode    : ('t, 'm) opcode
  ; component : component
  ; cycles    : int
  }
