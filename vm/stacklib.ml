
let stack = object
  val mutable s : int list = []

  method get = s
  method set int = s <- int

  method pop = match s with
    | hd :: intl ->
      s <- intl;
      Some hd
    | [] -> None

  method push x = s <- x::s

  method peek = match s with
    | hd :: _ -> Some hd
    | _ -> None

  method swap = match s with
    | intop :: nxt :: rest ->
      s <- (nxt :: intop :: rest)
    | _ -> ()

  method dup = match s with
    | x :: _ -> s <- (x::s)
    | _ -> ()
end

module API = struct

  exception StackDepth of int
  let insufficient_depth i = raise (StackDepth i)

  let depth stack = List.length stack#get

  let empty () = match stack#get with
    | [] -> true
    | _  -> false

  let bin_op fn = match stack#get with
    | intop :: next :: rest ->
      stack#set ((fn intop next)::rest)
    | _ -> insufficient_depth (depth stack)

  let un_op fn = match stack#get with
    | intop :: rest ->
      stack#set ((fn intop)::rest)
    | _ -> insufficient_depth (depth stack)

  let dup () = stack#dup

  let swap () = stack#swap

  let peek () = stack#peek

  let push x = stack#push x

  let pop () = stack#pop
end
