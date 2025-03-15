open Miniml.Lexer

let () =
  let r:regexp = Concat(
    Concat(
      char_regexp 'b',
      Alt(
        char_regexp 'a',
        char_regexp 'e')
    ),
      Star(str_regexp "be")) in
  let n, _ = nfa_of_regexp r 0 (fun _ -> Some(TK_ARROW)) in
  let oc: 'a BatInnerIO.output = BatIO.output_channel (open_out "/tmp/test.dot") in
  let _ = dot_of_nfa oc n in
  BatIO.close_out oc
;;
