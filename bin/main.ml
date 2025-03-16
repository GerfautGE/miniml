open Cli
open Lexer
open Parser

(* Miniml is a compiler for a mini-ML language *)
let () =
  (* retreive source code from files *)
  let sources = sources
  in List.iter (
    fun file -> file
    |> lexer
    |> parser
  ) sources
;;
