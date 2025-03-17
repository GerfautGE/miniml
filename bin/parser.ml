open Miniml.Token
open Miniml.Parser
open Miniml.Grammar
open Batteries


let parser (tks: token list) : unit =
  List.iter (fun t -> Printf.printf "%s\n" (string_of_token t)) tks;
  Printf.printf "TODO: parsing not implemented yet\n";
  let n = null (Nt Identifier) g in
  let _ = print_endline (dump n) in
  ()
