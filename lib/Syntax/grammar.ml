(* Context-free grammar *)
open Token
open Batteries

type nonterminal =
  | Prog
  | Identifier

type terminal = token

type symbol = Nt of nonterminal | T of terminal


type grammar = {
  n: nonterminal list;
  t: terminal list;
  s: nonterminal;
  r: (nonterminal * (symbol list)) list
}

(* Get rules for a non terminal *)
let rules (nt: nonterminal) (g: grammar): (symbol list) list =
  List.map snd (List.filter (fun (x, _) -> x = nt) g.r)


let null_t: ( nonterminal , bool) Hashtbl.t =  Hashtbl.create 7
let nullable (nt: nonterminal) = match Hashtbl.find_opt null_t nt with
  | Some v -> v
  | None -> false

(* Compute if [] can be derived from a nonterminal nt in g *)

let null_once (nt: nonterminal) (g: grammar) : bool =
  let rules = rules nt g in
  (* Check if [] is in the rule *)
  List.exists (fun x -> x = []) rules

(* Compute if [] can be derived recursively from sym in g *)
let null (_ : symbol) (_: grammar): bool = false

(* Compute if a terminal t can be in first position in a word derived from g *)
let first (_: terminal) (_: grammar): bool =
  false

(* Compute if a terminal can follow a nonterminal in a word derived from g *)
let follow (_: nonterminal) (_: grammar): terminal list =
  []
