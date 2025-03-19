(* Abstract Syntax Tree *)

type const =
  | Cint of int
  | Cbool of bool

type op =
  | Add
  | Mul

type node_kind =
  | Tidentifier of string
  | Tconstant of const
  | Top of op
  | Tanonfun
  | Tapply
  | Tpair
  | Tlocalbinding


type tree =
  | Node of node_kind * tree list
