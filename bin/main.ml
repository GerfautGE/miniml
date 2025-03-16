open Lexer

let () =
  let input = "let main = fun _ -> 0" in
  input
  |> lexer
;;
