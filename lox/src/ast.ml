type call_arg =
  | Arg_pos of expr
  | Arg_named of string * expr * int  (* name, value, line *)

and expr =
  | Binary of expr * Token.token * expr
  | Grouping of expr
  | Literal of Token.literal
  | Unary of Token.token * expr
  | Variable of string * int option * int                (* name, resolved depth, line *)
  | Assign of string * expr * int option * int           (* name, value, resolved depth, line *)
  | Call of expr * Token.token * call_arg list
  | Get of expr * string * int                           (* object, property name, line *)
  | Set of expr * string * expr * int                    (* object, property name, value, line *)
[@@deriving show]

type stmt =
  | Expression of expr
  | Print of expr
  | Var of string * expr option
  | If of expr * stmt * stmt option
  | Block of stmt list
  | While of expr * stmt
  | Fun of string * string list * stmt list
  | Class of string * (string * string list * stmt list) list
  | Return of Token.token * expr option
[@@deriving show]
