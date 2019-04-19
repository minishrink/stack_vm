
type t = int

type component =
  | Alu
  | Mem
  | Stk
  | Var

type var_op =
   | BIPUSH of t
   | SIPUSH of t
   | CONST of t

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

type unary_op =
  | NEG
  |

type 't opcode

type 't instruction =
  { opcode    : 't opcode
  ; component : component
  ; cycles    : int
  }

type 'm memory =
  | Load of 'm
  | Store of 'm
