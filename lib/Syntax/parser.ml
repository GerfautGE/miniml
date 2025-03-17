(* parser for miniml language *)

open Token
open Grammar

let id: string = "default"

let g: grammar = {
  n = [Prog; Identifier;];
  t = [TK_LET; TK_FUN; TK_EQUAL; TK_EOF];
  s = Prog;
  r = [
    (Prog,       [T TK_LET; Nt Identifier; T TK_EQUAL]);
    (Prog,       []);
    (Identifier, [T (TK_ID id)])
  ]
}
