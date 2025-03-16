open Miniml.Token
open Miniml.Regexp
open Batteries

(* List of regexp per tokens *)
let list_regexp : (regexp * (string -> token option)) list =
  [
    (str_regexp "let", fun _ -> Some(TK_LET));
    (str_regexp "fun", fun _ -> Some(TK_FUN));
    (char_regexp '=', fun _ -> Some(TK_EQUAL));
    (str_regexp "->", fun _ -> Some(TK_ARROW));
    (* int : (digit)+ *)
    (plus digit_r, fun s -> Some(TK_INT(int_of_string s)));
    (* id: ( '_' | lowercase)( '_' | alpha | digit)* *)
    ((char_regexp '_' || lowercase_r)++(Star(char_regexp '_' || alpha || digit_r)), fun s -> Some(TK_ID(s)));
    ((char_regexp ' ' || char_regexp '\n' || char_regexp '\t' || char_regexp '\r'), fun _ -> None);
    (Eps, fun _ -> Some(TK_EOF));
  ]
;;

let lexer (input: string): unit =
  match Miniml.Lexer.lexer list_regexp input with
  | _, w when w <> [] -> Printf.printf "Error: %s\n" (String.of_list w)
  | [], _ -> Printf.printf "No token found\n"
  | tokens, _ -> List.iter (fun t -> Printf.printf "%s\n" (string_of_token t)) tokens
;;
