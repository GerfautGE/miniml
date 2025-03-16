open Batteries
open Utils

type regexp =
  | Eps
  | Charset of char set
  | Concat of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp

(* Regexp helpers *)
let lowercase_r: regexp = Charset (Set.of_list (String.to_list"abcdefghijklmnopqrstuvwxyz"))
let uppercase_r: regexp = Charset (Set.of_list (String.to_list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
let alpha: regexp = Alt(lowercase_r, uppercase_r)
let digit_r: regexp = Charset (Set.of_list (String.to_list "0123456789"))

let plus (r: regexp) : regexp = Concat(r, Star(r))

let ( || ) (r1: regexp) (r2: regexp) = Alt(r1, r2)
let ( ++ ) (r1: regexp) (r2: regexp) = Concat(r1, r2)

(* Recognize one char c *)
let char_regexp (c: char): regexp =
  Charset( Set.singleton c)

(* Recognize a whole string s *)
let str_regexp (s: string): regexp =
  String.fold_right ( fun (c: char) r: regexp -> Concat(char_regexp c, r)) s Eps
