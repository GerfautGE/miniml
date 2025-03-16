open Cli
open Lexer

(* Miniml is a compiler for a mini-ML language *)

let () =
  (* retreive source code from files *)
  let sources = sources
  in List.iter (
    fun filename -> filename
    |> lexer
(*  |> parser
    |> ...
*)

  ) sources
;;
