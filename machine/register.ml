
module M = Memory

type register =
  | TOP
  | NEXT
  | THIRD
  | LBR
  | GBR
  | VBA
  | F

type register_machine =
  {  _TOP : M.memory_value ref
  ; _NEXT : M.memory_value ref
  ; _THIRD : M.memory_value ref
  ; _LBR : M.memory_value ref
  ; _GBR : M.memory_value ref
  ; _VBA : M.memory_value ref
  ; _F : M.memory_value ref
  }

let create_register_machine ?(n = 0) () =
  {  _TOP = ref n
  ; _NEXT = ref n
  ; _THIRD = ref n
  ; _LBR = ref n
  ; _GBR = ref n
  ; _VBA = ref n
  ; _F = ref n
  }

module type RegisterMachine = sig
  val machine : register_machine
end

module RegisterOp(R : RegisterMachine) = struct

  let get machine = function
    | TOP -> machine._TOP
    | NEXT -> machine._NEXT
    | THIRD -> machine._THIRD
    | LBR -> machine._LBR
    | GBR -> machine._GBR
    | VBA -> machine._VBA
    | F -> machine._F

  let read register =
    let register = get R.machine register in
    !register

  let write_to register new_value =
    let register = get R.machine register in
    register := new_value
end
