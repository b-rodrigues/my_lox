(* Expression types *)
type expr =
  | Binary of expr * Token.token * expr
  | Grouping of expr
  | Literal of Token.literal
  | Unary of Token.token * expr
  | Variable of string
[@@deriving show]

(* Statement types *)
type stmt =
  | Expression of expr
  | Print of expr
  | Var of string * expr option
[@@deriving show]
