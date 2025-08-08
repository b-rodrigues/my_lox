(* Expression types *)
type expr =
  | Binary of expr * Token.token * expr
  | Grouping of expr
  | Literal of Token.literal
  | Unary of Token.token * expr
  | Variable of string
  | Assign of string * expr
  | Call of expr * Token.token * expr list
[@@deriving show]

(* Statement types *)
type stmt =
  | Expression of expr
  | Print of expr
  | Var of string * expr option
  | If of expr * stmt * stmt option
  | Block of stmt list
  | While of expr * stmt
  | Fun of string * string list * stmt list
  | Return of Token.token * expr option
[@@deriving show]
