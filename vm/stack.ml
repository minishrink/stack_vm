
module Stack (V : Machine.Value) = struct

  exception StackDepth of int
  let stack_depth i = raise (StackDepth i)

  let stack = object
    val mutable s : V.t list = []

    method get = s
    method set t = s <- t

    method pop = match s with
      | hd :: tl ->
        s <- tl;
        Some hd
      | [] -> None

    method push x = s <- x::s

    method peek = match s with
      | hd :: _ -> Some hd
      | _ -> None

    method swap = match s with
      | top :: nxt :: rest ->
        s <- (nxt :: top :: rest)
      | _ -> ()

    method dup = match s with
      | x :: _ -> s <- (x::s)
      | _ -> ()
  end


  let depth () = List.length stack#get

  let empty = match stack#get with
    | [] -> true
    | _  -> false

  let bin_op fn = match stack#get with
    | top :: next :: rest ->
      stack#set ((fn top next)::rest)
    | lst -> stack_depth (depth ())

  let un_op fn = match stack#get with
    | top :: rest ->
      stack#set ((fn top)::rest)
    | lst -> stack_depth (depth ())

end
