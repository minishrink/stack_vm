
module P = Pervasives
module M = Memory
module J5_Machine = Machine.J5_Architecture

let j5 = J5_Machine.machine

exception StackFailure of M.memory_monad

let unwrap = function
  | M.Ok v -> v
  | (M.BadValue _) as err -> raise (StackFailure err)

let nop = ()

(** Pop top (and maybe next) off stack
 * Transform it
 * Push back to stack
 **)

let _add () = (+) j5._TOP j5._NEXT
let _sub x y = (-) x y

let _inc x = x + 1
let _dec x = x - 1

let _and x y = P.(land) x y
let _or x y = P.(lor) x y
let _not x = P.lnot x
let _xor x y = P.(lxor) x y

(** Shifting **)

let _shr x n = P.(lsr) x n
let _shl x n = P.(lsl) x n

(** Comparative **)

let _tgt x y = if x > y then 1 else 0
let _tlt x y = if x < y then 1 else 0
let _teq x y = if x = y then 1 else 0
let _tsz x = if x=0 then 1 else 0

