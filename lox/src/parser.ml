open Token
open Ast

exception ParseError of string * int

type parser = {
  tokens : token list;
  current : int;
}

let create_parser tokens = { tokens; current = 0 }

let is_at_end parser =
  match List.nth_opt parser.tokens parser.current with
  | Some token -> token.token_type = EOF
  | None -> true

let peek parser =
  match List.nth_opt parser.tokens parser.current with
  | Some token -> token
  | None -> failwith "No more tokens"

let previous parser =
  match List.nth_opt parser.tokens (parser.current - 1) with
  | Some token -> token
  | None -> failwith "No previous token"

let advance parser =
  if not (is_at_end parser) then
    { parser with current = parser.current + 1 }
  else
    parser

let check parser token_type =
  if is_at_end parser then false
  else (peek parser).token_type = token_type

let match_tokens parser token_types =
  List.exists (check parser) token_types

let consume parser token_type message =
  if check parser token_type then
    (advance parser, peek parser)
  else
    let current_token = peek parser in
    raise (ParseError (message, current_token.line))

(* Forward declarations for recursive parsing *)
let rec expression parser = equality parser
and equality parser =
  let rec loop parser left =
    if match_tokens parser [BANG_EQUAL; EQUAL_EQUAL] then
      let parser = advance parser in
      let operator = previous parser in
      let (parser, right) = comparison parser in
      loop parser (Binary (left, operator, right))
    else
      (parser, left)
  in
  let (parser, expr) = comparison parser in
  loop parser expr

and comparison parser =
  let rec loop parser left =
    if match_tokens parser [GREATER; GREATER_EQUAL; LESS; LESS_EQUAL] then
      let parser = advance parser in
      let operator = previous parser in
      let (parser, right) = term parser in
      loop parser (Binary (left, operator, right))
    else
      (parser, left)
  in
  let (parser, expr) = term parser in
  loop parser expr

and term parser =
  let rec loop parser left =
    if match_tokens parser [MINUS; PLUS] then
      let parser = advance parser in
      let operator = previous parser in
      let (parser, right) = factor parser in
      loop parser (Binary (left, operator, right))
    else
      (parser, left)
  in
  let (parser, expr) = factor parser in
  loop parser expr

and factor parser =
  let rec loop parser left =
    if match_tokens parser [SLASH; STAR] then
      let parser = advance parser in
      let operator = previous parser in
      let (parser, right) = unary parser in
      loop parser (Binary (left, operator, right))
    else
      (parser, left)
  in
  let (parser, expr) = unary parser in
  loop parser expr

and unary parser =
  if match_tokens parser [BANG; MINUS] then
    let parser = advance parser in
    let operator = previous parser in
    let (parser, right) = unary parser in
    (parser, Unary (operator, right))
  else
    primary parser

and primary parser =
  if match_tokens parser [FALSE] then
    let parser = advance parser in
    (parser, Literal (Lit_bool false))
  else if match_tokens parser [TRUE] then
    let parser = advance parser in
    (parser, Literal (Lit_bool true))
  else if match_tokens parser [NIL] then
    let parser = advance parser in
    (parser, Literal Lit_nil)
  else if match_tokens parser [NUMBER; STRING] then
    let parser = advance parser in
    let token = previous parser in
    (parser, Literal (Option.get token.literal))
  else if match_tokens parser [IDENTIFIER] then
    let parser = advance parser in
    let token = previous parser in
    (parser, Variable token.lexeme)
  else if match_tokens parser [LEFT_PAREN] then
    let parser = advance parser in
    let (parser, expr) = expression parser in
    let (parser, _) = consume parser RIGHT_PAREN "Expect ')' after expression." in
    (parser, Grouping expr)
  else
    let current_token = peek parser in
    raise (ParseError ("Expect expression.", current_token.line))

(* Statement parsing *)
let print_statement parser =
  let (parser, value) = expression parser in
  let (parser, _) = consume parser SEMICOLON "Expect ';' after value." in
  (parser, Print value)

let var_declaration parser =
  let (parser, name_token) = consume parser IDENTIFIER "Expect variable name." in
  let name = name_token.lexeme in
  let (parser, init_expr) =  (* Changed from 'initializer' to 'init_expr' *)
    if match_tokens parser [EQUAL] then
      let parser = advance parser in
      let (parser, expr) = expression parser in
      (parser, Some expr)
    else
      (parser, None)
  in
  let (parser, _) = consume parser SEMICOLON "Expect ';' after variable declaration." in
  (parser, Var (name, init_expr))  (* Updated here too *)

let expression_statement parser =
  let (parser, expr) = expression parser in
  let (parser, _) = consume parser SEMICOLON "Expect ';' after expression." in
  (parser, Expression expr)

let statement parser =
  if match_tokens parser [PRINT] then
    let parser = advance parser in
    print_statement parser
  else if match_tokens parser [VAR] then
    let parser = advance parser in
    var_declaration parser
  else
    expression_statement parser

let parse tokens =
  try
    let parser = create_parser tokens in
    let statements = ref [] in
    let parser = ref parser in
    while not (is_at_end !parser) do
      let (new_parser, stmt) = statement !parser in
      parser := new_parser;
      statements := stmt :: !statements
    done;
    Ok (List.rev !statements)
  with
  | ParseError (msg, line) -> Error (Printf.sprintf "Line %d: %s" line msg)
