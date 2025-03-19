open Miniml.Ast


let dot_of_ast (oc: out_channel) (_: tree) : unit =
  Printf.fprintf oc "digraph G {\n";
  (* Node Todo *)
  Printf.fprintf oc "Todo;\n";
  Printf.fprintf oc "}\n"

let dump_ast: unit =
  (* let x = 42 in fun y -> x + y 3 *)
  let le_tree = Node(Tlocalbinding, [Node(Tidentifier "x", []); Node(Tconstant (Cint 42), []); Node(Tanonfun, [Node(Tidentifier "y", []); Node(Top Add, [Node(Tidentifier "x", []); Node(Tidentifier "y", [])]); Node(Tconstant (Cint 3), [])])]) in
  let oc = open_out "/tmp/ast.dot" in
  let _ = dot_of_ast oc le_tree in
  close_out oc
