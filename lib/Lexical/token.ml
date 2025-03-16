type token =
  | TK_LET
  | TK_FUN
  | TK_EQUAL
  | TK_ARROW
  | TK_INT of int
  | TK_ID of string
  | TK_EOF

let string_of_token (t:token) =
  match t with
  | TK_LET -> "TK_LET"
  | TK_FUN -> "TK_FUN"
  | TK_EQUAL -> "TK_EQUAL"
  | TK_ARROW -> "TK_ARROW"
  | TK_INT i -> "TK_INT(" ^ (string_of_int i) ^ ")"
  | TK_ID s -> "TK_ID(" ^ s ^ ")"
  | TK_EOF -> "TK_EOF"
