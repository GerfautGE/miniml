(* Abstract Syntax Tree *)

type const =
  | Cint of int
  | Cbool of bool

let string_of_const = function
  | Cint i -> string_of_int i
  | Cbool b -> string_of_bool b

type op =
  | Add
  | Mul

let string_of_op = function
  | Add -> "+"
  | Mul -> "*"

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
