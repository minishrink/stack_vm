
module S = Stacklib
module M = Memory
module R = Register

module type Architecture = sig
  type stack_value
  type mem_value
  type register
  type stack_machine

  val machine : stack_machine
  val to_stack_value : string -> stack_value
  val to_mem_value : string -> mem_value
  val to_register : string -> register
end


module J5_Architecture = struct
  type value = M.memory_value

  type stack_machine =
    { stack : S.stack ref
    ; memory : M.memory
    ; registers : R.register_machine
    }

  let j5_machine =
    { stack = S.global_stack
    ; memory = M.create_memory 20
    ; registers = R.create_register_machine ()
    }

  let stack = j5_machine.stack
  let machine = j5_machine

  type stack_value = value
  type mem_value = value
  type register = R.register

  let to_stack_value = int_of_string
  let to_mem_value = int_of_string

  let to_register =
    let open R in
    function
    | "TOP" -> TOP
    | "NEXT" -> NEXT
    | "THIRD" -> THIRD
    | "LBR" -> LBR
    | "GBR" -> GBR
    | "VBA" -> VBA
    | "F" -> F
    | _ -> failwith "to_register"

  module Reg = R.RegisterOp(struct let machine = j5_machine.registers end)

  module Registers = struct
    open R
    open Reg
    let rotate_down () =
      (* NEXT -> THIRD *)
      write_to THIRD (read NEXT);
      (* TOP -> NEXT *)
      write_to NEXT (read TOP)

    let rotate_up () =
      (* NEXT -> THIRD *)
      write_to NEXT (read THIRD);
      (* TOP -> NEXT *)
      write_to TOP (read NEXT)
  end

  module Stack = struct
    let push value =
      Registers.rotate_down ();
      Reg.write_to R.TOP value;
      S.push value stack

    let pop ?(final=false) () =
      if final then Registers.rotate_up ();
      S.pop stack
  end
end

