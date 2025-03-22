open Miniml.Ast


let dot_of_ast (oc: out_channel) (root: tree) : unit =
  Printf.fprintf oc "digraph G {\n";
  Printf.fprintf oc "node [shape=record];\n";
  let rec aux (node: tree) : unit =
    match node with
    | Node(nk, children) ->
      let node_id = (Hashtbl.hash node) in
      let node_label = match nk with
        | Tidentifier s -> Printf.sprintf "Identifier %s" s
        | Tconstant c -> Printf.sprintf "Constant %s" (string_of_const c)
        | Top o -> Printf.sprintf "Op %s" (string_of_op o)
        | Tanonfun -> "AnonFun"
        | Tapply -> "Apply"
        | Tpair -> "Pair"
        | Tlocalbinding -> "LocalBinding"
      in
      Printf.fprintf oc "%d [label=\"%s\"];\n" node_id node_label;
      List.iter (fun child ->
          let child_id = (Hashtbl.hash child) in
          Printf.fprintf oc "%d -> %d;\n" node_id child_id;
          aux child
        ) children
  in
  aux root;
  Printf.fprintf oc "}\n"

let dump_ast: unit =
  (* let main = fun _ -> 0*)
  let le_tree =
    Node(Tlocalbinding, [
      Node(Tidentifier "main", []);
      Node(Tanonfun, [
        Node(Tidentifier "_", []);
        Node(Tconstant (Cint 0), [])
      ])
    ]) in
  let oc = open_out "/tmp/ast.dot" in
  let _ = dot_of_ast oc le_tree in
  close_out oc
