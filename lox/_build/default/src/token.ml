type token_type =
  (* Single-character tokens. *)
  | LEFT_PAREN | RIGHT_PAREN
  | LEFT_BRACE | RIGHT_BRACE
  | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR

  (* One or two character tokens. *)
  | BANG | BANG_EQUAL
  | EQUAL | EQUAL_EQUAL
  | GREATER | GREATER_EQUAL
  | LESS | LESS_EQUAL

  (* Literals. *)
  | IDENTIFIER | STRING | NUMBER

  (* Keywords. *)
  | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR
  | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE

  | EOF
[@@deriving show]

type literal =
  | Lit_number of float
  | Lit_string of string
  | Lit_bool of bool
  | Lit_nil
[@@deriving show]

type token = {
  token_type : token_type;
  lexeme : string;
  literal : literal option;
  line : int;
}
[@@deriving show]
