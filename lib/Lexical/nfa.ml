open Utils
open Token
open Regexp

(* Non-Determinitic Finite Automata (NFA) *)
(*
(Q,T,I,F) with :
  - Q: finite set of states
  - T: transitions from q \in Q to q' \in Q on a \in Alphabet (ASCII)
  - I: set of initial states
  - F: set of final states
*)
type n_state = int

type nfa = {
  q: n_state list;
  (* char set may be none for epsilon transitions *)
  t: n_state -> (char set option * n_state) list;
  i: n_state list;
  f: (n_state * (string -> token option)) list;
}


(* Concatenation of two NFA (n1n2) *)
let concat_nfa (n1: nfa) (n2: nfa): nfa =
  let q = n1.q @ n2.q in
  let i = n1.i in
  let f = n2.f in
  let t (st: n_state) =
    (* Need to mend the two NFAs at the frontier *)
    let stitch_out: n_state list = List.map fst n1.f in (* all n1's final states *)
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
  let t (st:n_state) =
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
let rec nfa_of_regexp (r: regexp) (curr_state: n_state) (t: string->token option): nfa * n_state =
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

(* Print NFA in dot format *)
let dot_of_nfa oc (n: nfa): unit =
  let _ = Batteries.Printf.fprintf oc "digraph nfa {\n" in
  let _ = Batteries.Printf.fprintf oc "rankdir=LR;\n" in
  let _ = Batteries.Printf.fprintf oc "size=\"8,5\";\n" in
  (* Print final states with double circle *)
  let _ = Batteries.Printf.fprintf oc "node [shape = doublecircle];\n" in
  let _ = Batteries.List.iter (fun (q, _) -> Batteries.Printf.fprintf oc "%d;\n" q) n.f in
  (* Print other states with circle *)
  let _ = Batteries.Printf.fprintf oc "node [shape = circle];\n" in
  let _ = Batteries.List.iter (fun (q) -> Batteries.Printf.fprintf oc "%d;\n" q) n.q in
  (* Arrow to initial state *)
  let _ = Batteries.Printf.fprintf oc "node [shape = point ]; start\n" in
  let _ = Batteries.Printf.fprintf oc "node [shape = circle ];\n" in
  let _ = Batteries.List.iter (fun q -> Batteries.Printf.fprintf oc "start -> %d;\n" q) n.i in
  (* Print transitions *)
  let _ = Batteries.List.iter (fun q ->
    match n.t q with
    | [] -> ()
    | l -> Batteries.List.iter (fun (c, q') ->
      match c with
      | None -> Batteries.Printf.fprintf oc "%d -> %d [label = \"ε\"];\n" q q'
      | Some c -> Batteries.Printf.fprintf oc "%d -> %d [label = \"%s\"];\n" q q' (Batteries.String.of_list (Batteries.Set.to_list c))
    ) l) n.q in
  Batteries.Printf.fprintf oc "}\n"
