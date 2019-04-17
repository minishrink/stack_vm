(* module CFG(I : Isa.Type) = struct *)
module CFG = struct
  (* module ISA = Isa.Instructions(I) *)

  module Node = struct
    type t = int
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)
  end

  module Edge = struct
    type t = string (* ISA.instruction *)
    let compare = Pervasives.compare
    let default = "ARC" (* ISA.NOP *)
    let to_string = Printf.sprintf "%s"
  end
end

module G = Graph.Imperative.Digraph.AbstractLabeled(CFG.Node)(CFG.Edge)

module X = struct
  include G
  let iter_vertex = G.iter_vertex
  let iter_edges = G.iter_edges
  let iter_edges_e = G.iter_edges_e
  let default_vertex_attributes _ = [`Shape `Box]
  let graph_attributes _ = [`Rankdir `TopToBottom]
  let vertex_name v = v |> V.label |> string_of_int
  let vertex_attributes = default_vertex_attributes
  let default_edge_attributes _ = [ `Arrowhead `Normal ]
  let edge_attributes = default_edge_attributes
  let get_subgraph _ = None
end

module P = Graph.Graphviz.Dot(X)

module GraphFunctions = struct
  let connect graph vertices =
    match vertices with
    | hd :: tl ->
      List.iter (G.add_edge graph hd) tl
    | _ -> ()

  let rec hyperconnect graph vertices =
    match vertices with
    | _ :: tl ->
      connect graph vertices;
      hyperconnect graph tl
    | [] -> ()

  let make_graph ?(n = 3) ?(hyper = false) () =
    let vertices = List.init n G.V.create in
    let graph = G.create () in
    List.iter (G.add_vertex graph) vertices;
    (if hyper then hyperconnect else connect) graph vertices;
    graph

  let print graph to_file =
    let file = open_out_bin to_file in
    P.output_graph file graph

end
