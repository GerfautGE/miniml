open Miniml.Token


let parser (tks: token list) : unit =
  List.iter (fun t -> Printf.printf "%s\n" (string_of_token t)) tks;
  Printf.printf "TODO: parsing not implemented yet\n"
