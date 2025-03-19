open Miniml.Regexp
open Miniml.Nfa
open Miniml.Dfa
open Miniml.Token
open Miniml.Lexer
open Batteries

let lowercase_letters = "abcdefghijklmnopqrstuvwxyz"
let uppercase_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let digits = "0123456789"
let other_characters = "?!=<>_ :;,{}()[]^`-+*/%@\n\t\x00.\"\'\\|~#$&"

let alphabet = String.to_list (lowercase_letters ^ uppercase_letters ^ digits ^ other_characters)

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


let dump_lexer: unit =
  let n = nfa_of_list_regexp list_regexp in
  let d = dfa_of_list_regexp list_regexp in
  let oc =  open_out "/tmp/nfa.dot" in
  let _ = dot_of_nfa oc n in
  let _ = close_out oc in
  let oc = open_out "/tmp/dfa.dot" in
  let _ = dot_of_dfa oc d alphabet in
  close_out oc
;;
