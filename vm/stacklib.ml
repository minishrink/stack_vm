
module Make (V : Machine.Value) = struct

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

  exception StackDepth of int
  let insufficient_depth i = raise (StackDepth i)

  let depth stack = List.length stack#get

  let empty = match stack#get with
    | [] -> true
    | _  -> false

  let bin_op fn = match stack#get with
    | top :: next :: rest ->
      stack#set ((fn top next)::rest)
    | _ -> insufficient_depth (depth stack)

  let un_op fn = match stack#get with
    | top :: rest ->
      stack#set ((fn top)::rest)
    | _ -> insufficient_depth (depth stack)

  let dup = stack#dup

  let swap = stack#swap

  let peek = stack#peek

  let push x = stack#push x

  let pop = stack#pop
end
