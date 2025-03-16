(* Lexer for miniml language *)
open Token
open Nfa
open Dfa
open Batteries
open Regexp

type lex_result =
  | TOKEN of token
  | BLANK
  | ERROR

let nfa_of_list_regexp l =
  let (n, _) = List.fold_left (fun (nfa, fs) (r,t) ->
      let n,fs = nfa_of_regexp r fs t in
      (alt_nfa nfa n, fs)
    ) ({ q = []; i = []; f = []; t = fun _ -> [] },1)
      l in n

let dfa_of_list_regexp l =
  let n = nfa_of_list_regexp l in
  dfa_of_nfa n

let tokenize_one (d:dfa) (w:char list) : lex_result * char list =
  let rec recognize (q: d_state) (w: char list) (current_token: char list) (last_accepted: lex_result * char list) : lex_result * char list =
      match w with
      |[] ->
        if fst last_accepted = ERROR then (TOKEN TK_EOF, []) else last_accepted
      |c::left_char -> match d.t q c with
        | None -> last_accepted
        | Some state -> let token = current_token@[c] in
          let finals = List.filter (fun (st, _) -> st = state) d.f in
          match finals with
          | [] -> recognize state left_char token last_accepted
          | fs::_ -> match snd fs (String.of_list token) with
            | None -> recognize state left_char token (BLANK, left_char)
            | Some t -> recognize state left_char token (TOKEN t, left_char)
    in
    recognize d.i w [] (ERROR, w)

let rec tokenize (d:dfa) (w:char list) : (token list * char list) =
  match tokenize_one d w with
  | ERROR, w -> [], w
  | BLANK, w -> tokenize d w
  | TOKEN t, w ->
    let (tks, w) =
    if t = TK_EOF then [], w
    else tokenize d w in
    t::tks, w

(* Lexer *)
let lexer (l : (regexp * (string -> token option))list) (input: string): token list * char list  =
  let d = dfa_of_list_regexp l in
  let w = String.to_list input in
  tokenize d w
;;
