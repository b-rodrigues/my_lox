(* lox/src/parser.ml *)

open Token
open Ast

exception ParseError of string * int

type parser = {
  tokens  : token list;
  current : int;
}

let create_parser tokens = { tokens; current = 0 }

let is_at_end parser =
  match List.nth_opt parser.tokens parser.current with
  | Some t -> t.token_type = EOF
  | None   -> true

let peek parser =
  match List.nth_opt parser.tokens parser.current with
  | Some t -> t
  | None   -> failwith "No more tokens"

let previous parser =
  match List.nth_opt parser.tokens (parser.current - 1) with
  | Some t -> t
  | None   -> failwith "No previous token"

let advance parser =
  if not (is_at_end parser) then
    { parser with current = parser.current + 1 }
  else
    parser

let check parser token_type =
  if is_at_end parser then false
  else (peek parser).token_type = token_type

let match_tokens parser types =
  List.exists (fun tt -> check parser tt) types

let consume parser token_type message =
  if check parser token_type then
    (advance parser, peek parser)
  else
    let t = peek parser in
    raise (ParseError (message, t.line))

(* --- Forwards --- *)
let rec declaration parser =
  if match_tokens parser [VAR] then
    let parser = advance parser in
    var_declaration parser
  else
    statement parser

and statement parser =
  if match_tokens parser [IF] then
    let parser = advance parser in
    let (parser, _)     = consume parser LEFT_PAREN  "Expect '(' after 'if'." in
    let (parser, cond)  = expression parser in
    let (parser, _)     = consume parser RIGHT_PAREN "Expect ')' after if condition." in

    let (parser, then_b) =
      if check parser LEFT_BRACE then
        let parser = advance parser in
        block parser
      else
        statement parser
    in

    let (parser, else_b) =
      if match_tokens parser [ELSE] then
        let parser = advance parser in
        if check parser LEFT_BRACE then
          let parser = advance parser in
          let (parser, b) = block parser in
          (parser, Some b)
        else
          let (parser, s) = statement parser in
          (parser, Some s)
      else
        (parser, None)
    in
    (parser, If (cond, then_b, else_b))

  else if match_tokens parser [PRINT] then
    let parser = advance parser in
    let (parser, value) = expression parser in
    let (parser, _) = consume parser SEMICOLON "Expect ';' after value." in
    (parser, Print value)

  else if check parser LEFT_BRACE then
    let parser = advance parser in
    block parser

  else
    let (parser, expr) = expression parser in
    let (parser, _) = consume parser SEMICOLON "Expect ';' after expression." in
    (parser, Expression expr)

and var_declaration parser =
  let (parser, name_tok) = consume parser IDENTIFIER "Expect variable name." in
  let name = name_tok.lexeme in
  let (parser, init_expr) =
    if match_tokens parser [EQUAL] then
      let parser = advance parser in
      let (parser, e) = expression parser in
      (parser, Some e)
    else
      (parser, None)
  in
  let (parser, _) = consume parser SEMICOLON "Expect ';' after variable declaration." in
  (parser, Var (name, init_expr))

and expression parser = logic_or parser

and logic_or parser =
  let rec loop parser left =
    if match_tokens parser [OR] then
      let parser = advance parser in
      let operator = previous parser in
      let (parser, right) = logic_and parser in
      loop parser (Binary (left, operator, right))
    else
      (parser, left)
  in
  let (parser, expr) = logic_and parser in
  loop parser expr

and logic_and parser =
  let rec loop parser left =
    if match_tokens parser [AND] then
      let parser = advance parser in
      let operator = previous parser in
      let (parser, right) = equality parser in
      loop parser (Binary (left, operator, right))
    else
      (parser, left)
  in
  let (parser, expr) = equality parser in
  loop parser expr

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
    let t = peek parser in
    raise (ParseError ("Expect expression.", t.line))

and block parser =
  let stmts = ref [] in
  let parser = ref parser in
  while not (check !parser RIGHT_BRACE) do
    let (p, stmt) = declaration !parser in
    parser := p;
    stmts := stmt :: !stmts
  done;
  let (parser, _) = consume !parser RIGHT_BRACE "Expect '}' after block." in
  (parser, Block (List.rev !stmts))

let parse tokens =
  let parser = create_parser tokens in
  try
    let rec loop parser acc =
      if is_at_end parser then
        List.rev acc
      else
        let (parser, stmt) = declaration parser in
        loop parser (stmt :: acc)
    in
    Ok (loop parser [])
  with ParseError (msg, line) ->
    Error (Printf.sprintf "Line %d: %s" line msg)
