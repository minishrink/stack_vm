
type stack = int list

type stack_result = Ok of int | EmptyStack

let global_stack : stack ref = ref []

let pop_off stack = match !stack with
  | _::b -> stack := b
  | _ -> ()

let pop stack = match !stack with
  | a::b ->
    stack := b;
    Ok a
  | _ ->
    EmptyStack

let push elem stack = stack := elem::(!stack)

let pop_push func stack = match !stack with
  | a::b -> stack := (func a)::b
  | _ -> ()
