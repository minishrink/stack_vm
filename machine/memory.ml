
(** Memory and wrapper types **)
type memory_value = int

type memory = memory_value array

type memory_monad = Ok of memory_value | BadValue of memory_value

(** Wrapper functions **)

let mem_success x = Ok x
let mem_failure x = BadValue x

let memory_operation f cell =
  try
    mem_success (f cell)
  with _ -> BadValue cell

(** Helpers **)

let create_memory n =
  try
    Array.make n 0
  with Invalid_argument _ ->
    Array.make 10 0


(** Memory API **)

module MemoryOp = struct

  let access memory cell =
    memory_operation (Array.get memory) cell

  let write memory cell value =
    try
      Array.set memory cell value
    with Invalid_argument _ -> ()
end
