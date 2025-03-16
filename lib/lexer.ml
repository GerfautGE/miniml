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

(* List of regexp per tokens *)
let list_regexp : (regexp * (string -> token option)) list =
  [
    (str_regexp "let", fun _ -> Some(TK_LET));
    (char_regexp '=', fun _ -> Some(TK_EQUAL));
    (str_regexp "->", fun _ -> Some(TK_ARROW));
    (* int : (digit)+ *)
    (plus digit_r, fun s -> Some(TK_INT(int_of_string s)));
    (* id: ( '_' | lowercase)( '_' | alpha | digit)* *)
    (char_regexp '_' || lowercase_r)++(Star(char_regexp '_' || alpha || digit_r)), fun s -> Some(TK_ID(s));
  ]
;;

(* Non-Determinitic Finite Automata (NFA) *)
(*
(Q,T,I,F) with :
  - Q: finite set of states
  - T: transitions from q \in Q to q' \in Q on a \in Alphabet (ASCII)
  - I: set of initial states
  - F: set of final states
*)
type state = int

type nfa = {
  q: state list;
  (* char set may be none for epsilon transitions *)
  t: state -> (char set option * state) list;
  i: state list;
  f: (state * (string -> token option)) list;
}


(* Concatenation of two NFA (n1n2) *)
let concat_nfa (n1: nfa) (n2: nfa): nfa =
  let q = n1.q @ n2.q in
  let i = n1.i in
  let f = n2.f in
  let t (st: state) =
    (* Need to mend the two NFAs at the frontier *)
    let stitch_out: state list = List.map fst n1.f in (* all n1's final states *)
    (* n2's inital states as well as all previous "natural" dest *)
    let stitch_in = n2.i@(List.map snd (n1.t st)) in
    if List.mem st stitch_out then
      List.map (fun q -> (None, q)) stitch_in (* let's add an epsilon transition *)
    else (* keep the transition from previous NFA *)
    if List.mem st n1.q then
      n1.t st
    else
      n2.t st in
  {q; t; i; f}
  ;;

(* Alternative of two NFA (n1|n2) *)
let alt_nfa (n1:nfa) (n2:nfa): nfa =
  let q = n1.q @ n2.q in
  let i = n1.i @ n2.i in
  let f = n1.f @ n2.f in
  (* Follow natural flow *)
  let t (st:state) =
    if List.mem st n1.q then
      n1.t st
    else
      n2.t st
  in
  {q; t; i; f}
;;

(* Star of NFA (n)* *)
let star_nfa (n:nfa) (t: string -> token option) : nfa =
  let q = n.q in
  let i = List.map fst n.f in
  let f = List.map (fun q -> (q, t)) n.i in
  let t st =
    if List.mem st i then
    (* Epsilon transition to all initial and natural dest *)
      List.map (fun q -> (None, q)) n.i@(n.t st)
    else
      n.t st in
  {q; t; i; f}

(* Convert a regexp to an NFA *)
let rec nfa_of_regexp (r: regexp) (curr_state: state) (t: string->token option): nfa * state =
  match r with
    | Eps -> {
        q = [curr_state];
        t = (fun _ -> []);
        i = [curr_state];
        f = [curr_state, t];}, curr_state + 1
    | Charset c -> {
      q = [curr_state; curr_state+1];
      t = (fun q -> if q = curr_state then
                      [Some(c), curr_state + 1]
                    else []);
      i = [curr_state];
      f =  [curr_state + 1, t];}, curr_state + 2
    | Concat(r1,r2) ->
      let n1, c_st1 = nfa_of_regexp r1 curr_state (fun _ -> None) in
      let n2, c_st2 = nfa_of_regexp r2 c_st1 t in
      concat_nfa n1 n2, c_st2
    | Alt(r1,r2) ->
      let n1, c_st1 = nfa_of_regexp r1 curr_state (fun _ -> None) in
      let n2, c_st2 = nfa_of_regexp r2 c_st1 t in
      alt_nfa n1 n2, c_st2
    | Star(r) ->
      let n, c_st = nfa_of_regexp r curr_state (fun _ -> None) in
      star_nfa n t, c_st

let charset_of_string (s: string): char set =
  Set.of_list (String.to_list s)

(* Print NFA in dot format *)
let dot_of_nfa oc (n: nfa): unit =
  let _ = Printf.fprintf oc "digraph nfa {\n" in
  let _ = Printf.fprintf oc "rankdir=LR;\n" in
  let _ = Printf.fprintf oc "size=\"8,5\";\n" in
  (* Print final states with double circle *)
  let _ = Printf.fprintf oc "node [shape = doublecircle];\n" in
  let _ = List.iter (fun (q, _) -> Printf.fprintf oc "%d;\n" q) n.f in
  (* Print other states with circle *)
  let _ = Printf.fprintf oc "node [shape = circle];\n" in
  let _ = List.iter (fun (q) -> Printf.fprintf oc "%d;\n" q) n.q in
  (* Arrow to initial state *)
  let _ = Printf.fprintf oc "node [shape = point ]; start\n" in
  let _ = Printf.fprintf oc "node [shape = circle ];\n" in
  let _ = List.iter (fun q -> Printf.fprintf oc "start -> %d;\n" q) n.i in
  (* Print transitions *)
  let _ = List.iter (fun q ->
    match n.t q with
    | [] -> ()
    | l -> List.iter (fun (c, q') ->
      match c with
      | None -> Printf.fprintf oc "%d -> %d [label = \"ε\"];\n" q q'
      | Some c -> Printf.fprintf oc "%d -> %d [label = \"%s\"];\n" q q' (String.of_list (Set.to_list c))
    ) l) n.q in
  Printf.fprintf oc "}\n"

(* Lexer *)
let lexer (input: string): unit =
  let _ = input in
  let _ = Printf.printf "TODO\n" in
  let _ = Printf.printf "%s\n" input in
  ()
;;
