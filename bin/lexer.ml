(* Lexer for miniml language *)
open Batteries

type 'a set = 'a Set.t

type token =
  | TK_LET
  | TK_EQUAL
  | TK_ARROW
  | TK_INT of int
  | TK_ID of string


type regexp =
  | Eps
  | Charset of char set
  | Concat of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp
  | Plus of regexp

(* Regexp helpers *)
let lowercase_r: regexp = Charset (Set.of_list (String.to_list"abcdefghijklmnopqrstuvwxyz"))
let uppercase_r: regexp = Charset (Set.of_list (String.to_list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
let alpha: regexp = Alt(lowercase_r, uppercase_r)
let digit_r: regexp = Charset (Set.of_list (String.to_list "0123456789"))

(* Recognize one char c *)
let char_regexp (c: char): regexp =
  Charset( Set.singleton c)

(* Recognize a whole string s *)
let str_regexp (s: string): regexp =
  String.fold_right ( fun (c: char) r: regexp -> Concat(char_regexp c, r)) s Eps

(* List of regexp per tokens *)
let list_regexp : (regexp * (string -> token)) list =
  [
    (str_regexp "let", fun _ -> TK_LET);
    (char_regexp '=', fun _ -> TK_EQUAL);
    (str_regexp "->", fun _ -> TK_ARROW);
    (* int : (digit)+ *)
    (Plus(
      digit_r
    ), fun s -> TK_INT(int_of_string s));
    (* id: ( '_' | lowercase)( '_' | alpha | digit)* *)
    (Concat(
      Alt(
        char_regexp '_',
        lowercase_r
      ),
      Star(
        Alt(
          Alt(
            char_regexp '_',
            alpha
          ),
          digit_r
        )
      )
    ), fun s -> TK_ID(s));
  ]
;;


let lexer (input: string): unit =
  let _ = input in
  let _ = Printf.printf "TODO\n" in
  Printf.printf "%s\n" input
;;
