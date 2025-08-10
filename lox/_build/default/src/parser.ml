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

let peek_next parser =
  match List.nth_opt parser.tokens (parser.current + 1) with
  | Some t -> t
  | None -> peek parser

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
    let tok = peek parser in
    (advance parser, tok)
  else
    let t = peek parser in
    raise (ParseError (message, t.line))

(* Forward declarations *)
let rec declaration parser =
  if match_tokens parser [CLASS] then
    class_declaration (advance parser)
  else if match_tokens parser [FUN] then
    fun_declaration (advance parser)
  else if match_tokens parser [VAR] then
    var_declaration (advance parser)
  else
    statement parser

and class_declaration parser =
  let (parser, name_tok) = consume parser IDENTIFIER "Expect class name." in
  let class_name = name_tok.lexeme in
  (* Optional superclass *)
  let (parser, superclass_expr_opt) =
    if match_tokens parser [LESS] then
      let parser = advance parser in
      let (parser, super_tok) = consume parser IDENTIFIER "Expect superclass name after '<'." in
      let super_expr = Variable (super_tok.lexeme, None, super_tok.line) in
      (parser, Some super_expr)
    else
      (parser, None)
  in
  let (parser, _) = consume parser LEFT_BRACE "Expect '{' before class body." in
  let rec methods_loop parser acc =
    if check parser RIGHT_BRACE || is_at_end parser then
      (parser, List.rev acc)
    else
      let (parser, m_name_tok) = consume parser IDENTIFIER "Expect method name." in
      let m_name = m_name_tok.lexeme in
      let (parser, _) = consume parser LEFT_PAREN "Expect '(' after method name." in
      let rec params_loop parser accp =
        if check parser RIGHT_PAREN then (parser, List.rev accp)
        else
          let (p, param_tok) = consume parser IDENTIFIER "Expect parameter name." in
          let accp = param_tok.lexeme :: accp in
          if match_tokens p [COMMA] then
            params_loop (advance p) accp
          else
            (p, List.rev accp)
      in
      let (parser, params) = params_loop parser [] in
      let (parser, _) = consume parser RIGHT_PAREN "Expect ')' after parameters." in
      let (parser, body_block) =
        if check parser LEFT_BRACE then
          block (advance parser)
        else
          let t = peek parser in
          raise (ParseError ("Expect '{' before method body.", t.line))
      in
      let body_stmts =
        match body_block with
        | Block stmts -> stmts
        | _ -> failwith "Method body not a block"
      in
      methods_loop parser ((m_name, params, body_stmts) :: acc)
  in
  let (parser, methods) = methods_loop parser [] in
  let (parser, _) = consume parser RIGHT_BRACE "Expect '}' after class body." in
  (parser, Class (class_name, superclass_expr_opt, methods))

and statement parser =
  if match_tokens parser [IF] then
    let parser = advance parser in
    let (parser, _)    = consume parser LEFT_PAREN  "Expect '(' after 'if'." in
    let (parser, cond) = expression parser in
    let (parser, _)    = consume parser RIGHT_PAREN "Expect ')' after if condition." in
    let (parser, then_b) =
      if check parser LEFT_BRACE then
        block (advance parser)
      else
        statement parser
    in
    let (parser, else_b) =
      if match_tokens parser [ELSE] then
        let parser = advance parser in
        if check parser LEFT_BRACE then
          let (parser, b) = block (advance parser) in
          (parser, Some b)
        else
          let (parser, s) = statement parser in
          (parser, Some s)
      else
        (parser, None)
    in
    (parser, If (cond, then_b, else_b))

  else if match_tokens parser [FOR] then
    let parser = advance parser in
    let (parser, _) = consume parser LEFT_PAREN "Expect '(' after 'for'." in
    let (parser, init_stmt) =
      if match_tokens parser [SEMICOLON] then
        (advance parser, None)
      else if match_tokens parser [VAR] then
        let (p, stmt) = var_declaration (advance parser) in
        (p, Some stmt)
      else
        let (p, expr) = expression parser in
        let (p, _) = consume p SEMICOLON "Expect ';' after loop initializer." in
        (p, Some (Expression expr))
    in
    let (parser, cond_expr) =
      if match_tokens parser [SEMICOLON] then
        (advance parser, Literal (Lit_bool true))
      else
        let (p, expr) = expression parser in
        let (p, _) = consume p SEMICOLON "Expect ';' after loop condition." in
        (p, expr)
    in
    let (parser, incr_stmt) =
      if match_tokens parser [RIGHT_PAREN] then
        (parser, None)
      else
        let (p, expr) = expression parser in
        let (p, _) = consume p RIGHT_PAREN "Expect ')' after for clauses." in
        (p, Some (Expression expr))
    in
    let (parser, body_stmt) =
      if check parser LEFT_BRACE then
        block (advance parser)
      else
        statement parser
    in
    let loop_body =
      match incr_stmt with
      | None -> body_stmt
      | Some inc -> Block [body_stmt; inc]
    in
    let full_loop = While (cond_expr, loop_body) in
    let stmts =
      match init_stmt with
      | None -> [full_loop]
      | Some s -> [s; full_loop]
    in
    (parser, Block stmts)

  else if match_tokens parser [WHILE] then
    let parser = advance parser in
    let (parser, _)    = consume parser LEFT_PAREN  "Expect '(' after 'while'." in
    let (parser, cond) = expression parser in
    let (parser, _)    = consume parser RIGHT_PAREN "Expect ')' after while condition." in
    let (parser, body) =
      if check parser LEFT_BRACE then
        block (advance parser)
      else
        statement parser
    in
    (parser, While (cond, body))

  else if match_tokens parser [RETURN] then
    let parser = advance parser in
    let ret_tok = previous parser in
    let (parser, value_opt) =
      if check parser SEMICOLON then (parser, None)
      else
        let (p, e) = expression parser in
        (p, Some e)
    in
    let (parser, _) = consume parser SEMICOLON "Expect ';' after return value." in
    (parser, Return (ret_tok, value_opt))

  else if match_tokens parser [PRINT] then
    let parser = advance parser in
    let (parser, value) = expression parser in
    let (parser, _) = consume parser SEMICOLON "Expect ';' after value." in
    (parser, Print value)

  else if check parser LEFT_BRACE then
    block (advance parser)

  else
    let (parser, expr) = expression parser in
    let (parser, _) = consume parser SEMICOLON "Expect ';' after expression." in
    (parser, Expression expr)

and fun_declaration parser =
  let (parser, name_tok) = consume parser IDENTIFIER "Expect function name." in
  let name = name_tok.lexeme in
  let (parser, _) = consume parser LEFT_PAREN "Expect '(' after function name." in
  let rec params_loop parser acc =
    if check parser RIGHT_PAREN then (parser, List.rev acc)
    else
      let (p, param_tok) = consume parser IDENTIFIER "Expect parameter name." in
      let acc = param_tok.lexeme :: acc in
      if match_tokens p [COMMA] then
        params_loop (advance p) acc
      else
        (p, List.rev acc)
  in
  let (parser, params) = params_loop parser [] in
  let (parser, _) = consume parser RIGHT_PAREN "Expect ')' after parameters." in
  let (parser, body_stmts) =
    if check parser LEFT_BRACE then
      let (p, blk) = block (advance parser) in
      match blk with
      | Block stmts -> (p, stmts)
      | _ -> failwith "block did not return Block"
    else
      let t = peek parser in
      raise (ParseError ("Expect '{' before function body.", t.line))
  in
  (parser, Fun (name, params, body_stmts))

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

and expression parser = assignment parser

and assignment parser =
  let (parser, lhs) = logic_or parser in
  if match_tokens parser [EQUAL] then
    let parser = advance parser in
    let (parser, rhs) = assignment parser in
    (match lhs with
     | Variable (name, _, line) -> (parser, Assign (name, rhs, None, line))
     | Get (obj, name, line) -> (parser, Set (obj, name, rhs, line))
     | _ ->
         let t = peek parser in
         raise (ParseError ("Invalid assignment target.", t.line)))
  else
    (parser, lhs)

and logic_or parser =
  let rec loop parser left =
    if match_tokens parser [OR] then
      let parser = advance parser in
      let operator = previous parser in
      let (parser, right) = logic_and parser in
      loop parser (Binary (left, operator, right))
    else parser, left
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
    else parser, left
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
    else parser, left
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
    else parser, left
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
    else parser, left
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
    else parser, left
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
    call parser

and call parser =
  let (parser, callee) = primary parser in
  let rec loop parser expr =
    if match_tokens parser [LEFT_PAREN] then
      let parser = advance parser in
      let rec args_loop parser acc seen_named =
        if check parser RIGHT_PAREN then (parser, List.rev acc)
        else
          let t1 = peek parser in
          let t2 = peek_next parser in
          if t1.token_type = IDENTIFIER && t2.token_type = EQUAL then
            (* named argument *)
            let parser = advance parser in
            let name_tok = previous parser in
            let name = name_tok.lexeme in
            let parser = advance parser (* consume '=' *) in
            let (parser, value_expr) = expression parser in
            let acc = Arg_named (name, value_expr, name_tok.line) :: acc in
            let seen_named = true in
            if match_tokens parser [COMMA] then
              args_loop (advance parser) acc seen_named
            else
              (parser, List.rev acc)
          else
            (* positional *)
            let (parser, arg_expr) = expression parser in
            if seen_named then
              let t = peek parser in
              raise (ParseError ("Positional arguments cannot follow named arguments.", t.line))
            else
              let acc = Arg_pos arg_expr :: acc in
              if match_tokens parser [COMMA] then
                args_loop (advance parser) acc seen_named
              else
                (parser, List.rev acc)
      in
      let (parser, args) = args_loop parser [] false in
      let (parser, _) = consume parser RIGHT_PAREN "Expect ')' after arguments." in
      let paren = previous parser in
      loop parser (Call (expr, paren, args))
    else if match_tokens parser [DOT] then
      let parser = advance parser in
      let (parser, name_tok) = consume parser IDENTIFIER "Expect property name after '.'." in
      loop parser (Get (expr, name_tok.lexeme, name_tok.line))
    else
      (parser, expr)
  in
  loop parser callee

and primary parser =
  if match_tokens parser [FALSE] then
    let parser = advance parser in (parser, Literal (Lit_bool false))
  else if match_tokens parser [TRUE] then
    let parser = advance parser in (parser, Literal (Lit_bool true))
  else if match_tokens parser [NIL] then
    let parser = advance parser in (parser, Literal Lit_nil)
  else if match_tokens parser [THIS] then
    let parser = advance parser in
    let tok = previous parser in
    (parser, Variable ("this", None, tok.line))
  else if match_tokens parser [SUPER] then
    let parser = advance parser in
    let super_tok = previous parser in
    let (parser, _) = consume parser DOT "Expect '.' after 'super'." in
    let (parser, method_tok) = consume parser IDENTIFIER "Expect superclass method name." in
    (parser, Super (super_tok, method_tok.lexeme, method_tok.line, None))
  else if match_tokens parser [NUMBER; STRING] then
    let parser = advance parser in
    let token = previous parser in
    (parser, Literal (Option.get token.literal))
  else if match_tokens parser [IDENTIFIER] then
    let parser = advance parser in
    let token = previous parser in
    (parser, Variable (token.lexeme, None, token.line))
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
  let parser_ref = ref parser in
  while not (check !parser_ref RIGHT_BRACE) do
    let (p, stmt) = declaration !parser_ref in
    parser_ref := p;
    stmts := stmt :: !stmts
  done;
  let (parser, _) = consume !parser_ref RIGHT_BRACE "Expect '}' after block." in
  (parser, Block (List.rev !stmts))

let parse tokens =
  let parser = create_parser tokens in
  try
    let rec loop parser acc =
      if is_at_end parser then List.rev acc
      else
        let (parser, stmt) = declaration parser in
        loop parser (stmt :: acc)
    in
    Ok (loop parser [])
  with
  | ParseError (msg, line) -> Error (Printf.sprintf "Line %d: %s" line msg)
