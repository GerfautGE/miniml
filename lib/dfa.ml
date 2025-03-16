open Utils
open Token
open Nfa
open Batteries

(* Deterministic Finite Automata *)
(*
(Q,T,I,F) with :
  - Q: finite set of states
  - T: transitions from q \in Q to q' \in Q on a \in Alphabet (ASCII)
  - I: set of initial states
  - F: set of final states
*)
type d_state = int set

type dfa = {
  q: d_state list;
  t: d_state -> char -> d_state option;
  i: d_state;
  f: (d_state * (string ->token option)) list;
}

(* Convert an NFA to a DFA *)

(* First step is to compute epsilon closure *)
let eps_closure (n:nfa) (st: n_state): n_state set =
  (* follow only eps-transitions from st in n *)
  let rec traversal (visisted_states: n_state set) (st: n_state) : n_state set =
    if Set.mem st visisted_states then
      (* We've already been there before *)
      visisted_states
    else
      let visisted_states = Set.add st visisted_states in
      let visisted_states = List.fold_left (
        fun visited (q, st') ->
          if q=None then
           (* Epsilon transition here *)
            traversal visited st'
          else
            (* Discard this path *)
            visisted_states)
      visisted_states (n.t st) in
      visisted_states in
  traversal Set.empty st

(* Compute epsilon closure union for set *)
let eps_closure_union (n: nfa) (set: n_state set): n_state set =
  Batteries.Set.fold( fun state visited -> Batteries.Set.union visited (eps_closure n state)) set Batteries.Set.empty

(* Compute initial states of DFA from NFA n *)
let dfa_initial (n: nfa): d_state =
  eps_closure_union n (Batteries.Set.of_list n.i)

(* DFA transition table *)
(* see https://en.wikipedia.org/wiki/State-transition_table *)

(* Helpers *)
let assoc_throw_none (l : ('a option * 'b) list) : ('a * 'b) list =
  Batteries.List.filter_map (fun (o,n) ->
      match o with
        None -> None
      | Some x -> Some (x,n)
    ) l

let assoc_distribute_key (l : ('a set * 'b) list) : ('a * 'b) list =
  Batteries.List.fold_left (fun (acc : ('a * 'b) list) (k, v) ->
      Batteries.Set.fold (fun c acc -> (c, v)::acc) k acc)
    [] l

let assoc_merge_vals (l : ('a * 'b) list) : ('a * 'b set) list =
  Batteries.List.fold_left (fun (acc : ('a * 'b set) list) (k, v) ->
      match Batteries.List.assoc_opt k acc with
      | None -> (k, Batteries.Set.singleton v)::acc
      | Some vl -> (k, Batteries.Set.add v vl)::Batteries.List.remove_assoc k acc
    ) [] l

(* Build the transition table of the DFA *)
let rec build_dfa_transition_table
  (table: (d_state, (char*d_state) list) Hashtbl.t)
  (n: nfa)
  (st: d_state) : unit =
  match Hashtbl.find_option table st with
  | Some _ -> ()
  | None ->
    let transitions : (char *d_state) list =
    st  |> Set.elements
        |> List.map (fun s -> n.t s)
        |> List.concat
        |> assoc_throw_none
        |> assoc_distribute_key
        |> assoc_merge_vals
        |> List.map (fun (c, st) -> c, eps_closure_union n st)
    in
    Hashtbl.replace table st transitions;
    List.iter (build_dfa_transition_table table n) (List.map snd transitions)

let prio (t:token) : int = match t with
  (* lower is better *)
  | TK_EOF -> 2
  | TK_ID _ -> 1
  | _ -> 0

let min_priority (l: token list) : token option =
  match l with
  | [] -> None
| head_tk :: left_tks -> let (t, _) =
Batteries.List.fold_left(fun (t1, p1) t2 ->
  if (prio t2 < p1) then
    (t2, prio t2)
  else
    (t1, p1)
) (head_tk, prio head_tk) left_tks in Some t


let dfa_final_states (n: nfa) (dfa_states: d_state list) : (d_state * (string -> token option)) list =
  (* We will process states one at a time *)
  let token_from_state (state: d_state) : int * d_state * (string -> token option) =
    (* We retrieve the final states of the nfa which are in the studied state of the DFA *)
    let nf_in_dstate = Batteries.List.filter (fun (q,_) -> Batteries.Set.mem q state) n.f in
    let d_t (str: string) : token option = (* We retrieve the token associated with the final state of the nfa *)
      let n_t = Batteries.List.map snd nf_in_dstate in (* We extract their token from the final states of the nfa *)
      let nfa_t_options = Batteries.List.map (fun t -> t str) n_t in (* We apply the token to the character string passed as an argument *)
      let nfa_tokens =
        Batteries.List.fold_left (fun acc elt ->
            match elt with
              None -> acc (* Discard unrecognized tokens *)
            | Some token -> token :: acc )
          [] nfa_t_options in
      min_priority nfa_tokens in (* keep only the token with minimal priority *)
    (Batteries.List.length nf_in_dstate, state, d_t) in

  let tokens_from_dfa = Batteries.List.map token_from_state dfa_states in (* We apply the token_from_state function to each state of the DFA *)
  let tokens = Batteries.List.filter (fun (cnt, _, _) -> cnt <> 0) tokens_from_dfa in
  Batteries.List.map (fun (_, state, dfa_t) -> (state, dfa_t)) tokens

let make_dfa_step (table: (d_state, (char * d_state) list) Batteries.Hashtbl.t) =
  fun (ds: d_state) (a: char) ->
  let lst = Batteries.Hashtbl.find_option table ds in
  match lst with
       |None -> None
       |Some l -> match Batteries.List.filter (fun x -> fst x=a) l with
         |[] -> None
         |(_,state)::_ -> Some state

(* Convert an NFA to a DFA *)
let dfa_of_nfa (n: nfa) : dfa =
  let table : (d_state, (char * d_state) list) Batteries.Hashtbl.t =
    Batteries.Hashtbl.create (Batteries.List.length n.q) in
  let initial = dfa_initial n in
  build_dfa_transition_table table n initial;
  let states = Batteries.Hashtbl.keys table |> Batteries.List.of_enum in
  let final = dfa_final_states n states in
  let transition = make_dfa_step table in
  {
    q = states;
    t = transition;
    i = initial;
    f = final;
  }

(* Print DFA in dot format *)

let string_of_int_list l =
  Printf.sprintf "%s" (String.concat "_" (List.map string_of_int l))

let string_of_int_set s =
  string_of_int_list (Set.to_list s)

let char_list_to_char_ranges s =
  let rec recognize_range (cl: int list) l opt_c n =
    match cl with
    | [] -> (match opt_c with
          None -> l
        | Some c -> (c,n)::l
      )
    | c::r -> (match opt_c with
        | None -> recognize_range r l (Some c) 0
        | Some c' ->
          if c' + n + 1 = c
          then recognize_range r l (Some c') (n + 1)
          else recognize_range r ((c',n)::l) (Some c) 0
      )
  in
  let l = recognize_range (List.sort Stdlib.compare (List.map Char.code s)) [] None 0 in
  let escape_char c =
    if c = '"' then "\\\""
    else if c = '\\' then "\\\\"
    else if c = '\x00' then "\\\\0"
    else if c = '\t' then "\\\\t"
    else if c = '\n' then "\\\\n"
    else Printf.sprintf "%c" c in
  List.fold_left (fun acc (c,n) ->
      match n with
      | 0 -> Printf.sprintf "%s%s" (escape_char (Char.chr c)) acc
      | 1 -> Printf.sprintf "%s%s%s" (escape_char (Char.chr c)) (c + 1 |> Char.chr |> escape_char) acc
      | _ -> Printf.sprintf "%s-%s%s" (escape_char (Char.chr c))
          (escape_char (Char.chr (c + n))) acc
    ) "" l

let dot_of_dfa oc (d : dfa) (cl: char list): unit =
  Printf.fprintf oc "digraph {\n";
  Printf.fprintf oc "N%s [shape=\"house\" color=\"red\"]\n" (string_of_int_set d.i);
  List.iter (fun (q,t) ->
      Printf.fprintf oc "N%s [shape=\"rectangle\", label=\"%s\"]\n"
        (string_of_int_set q) (match t "0" with | Some t -> string_of_token t | None -> "" )) d.f;
  List.iter (fun q ->
      let l = List.fold_left (fun l a ->
          match d.t q a with
            None -> l
          | Some q' ->
            match List.assoc_opt q' l with
            | None -> (q', [a])::l
            | Some ql -> (q', a::ql)::List.remove_assoc q' l
        ) [] cl in
      List.iter (fun (q', cl) ->
          Printf.fprintf oc "N%s -> N%s [label=\"[%s]\"]\n"
            (string_of_int_set q)
            (string_of_int_set q') (char_list_to_char_ranges cl)
        ) l;
    ) d.q;
  Printf.fprintf oc "}\n"
