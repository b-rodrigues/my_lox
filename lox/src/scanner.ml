open Token

exception ScanError of string * int

type scanner = {
  source : string;
  tokens : token list;
  start : int;
  current : int;
  line : int;
}

let create_scanner source = {
  source;
  tokens = [];
  start = 0;
  current = 0;
  line = 1;
}

let is_at_end scanner = 
  scanner.current >= String.length scanner.source

let advance scanner =
  if is_at_end scanner then 
    (scanner, '\000')
  else
    let c = scanner.source.[scanner.current] in
    ({ scanner with current = scanner.current + 1 }, c)

let peek scanner =
  if is_at_end scanner then '\000'
  else scanner.source.[scanner.current]

let peek_next scanner =
  if scanner.current + 1 >= String.length scanner.source then '\000'
  else scanner.source.[scanner.current + 1]

let match_char scanner expected =
  if is_at_end scanner then (scanner, false)
  else if scanner.source.[scanner.current] <> expected then (scanner, false)
  else ({ scanner with current = scanner.current + 1 }, true)

let add_token scanner token_type literal =
  let text = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
  let token = {
    token_type;
    lexeme = text;
    literal;
    line = scanner.line;
  } in
  { scanner with tokens = token :: scanner.tokens }

let add_simple_token scanner token_type =
  add_token scanner token_type None

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alpha = function
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let is_alphanumeric c = is_alpha c || is_digit c

let scan_string scanner =
  let rec loop scanner =
    if is_at_end scanner then
      raise (ScanError ("Unterminated string", scanner.line))
    else
      match peek scanner with
      | '"' -> scanner
      | '\n' -> loop { scanner with line = scanner.line + 1; current = scanner.current + 1 }
      | _ -> loop { scanner with current = scanner.current + 1 }
  in
  let scanner = loop scanner in
  if is_at_end scanner then
    raise (ScanError ("Unterminated string", scanner.line))
  else
    (* Consume the closing quote *)
    let scanner = { scanner with current = scanner.current + 1 } in
    (* Extract the string value (without the quotes) *)
    let value = String.sub scanner.source (scanner.start + 1) (scanner.current - scanner.start - 2) in
    add_token scanner STRING (Some (Lit_string value))

let scan_number scanner =
  let rec consume_digits scanner =
    if is_digit (peek scanner) then
      consume_digits { scanner with current = scanner.current + 1 }
    else
      scanner
  in
  let scanner = consume_digits scanner in
  (* Look for a fractional part *)
  let scanner = 
    if peek scanner = '.' && is_digit (peek_next scanner) then
      (* Consume the "." *)
      let scanner = { scanner with current = scanner.current + 1 } in
      (* Consume fractional digits *)
      consume_digits scanner
    else
      scanner
  in
  let text = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
  let value = Float.of_string text in
  add_token scanner NUMBER (Some (Lit_number value))

let keywords = [
  ("and", AND);
  ("class", CLASS);
  ("else", ELSE);
  ("false", FALSE);
  ("for", FOR);
  ("fun", FUN);
  ("if", IF);
  ("nil", NIL);
  ("or", OR);
  ("print", PRINT);
  ("return", RETURN);
  ("super", SUPER);
  ("this", THIS);
  ("true", TRUE);
  ("var", VAR);
  ("while", WHILE);
]

let scan_identifier scanner =
  let rec loop scanner =
    if is_alphanumeric (peek scanner) then
      loop { scanner with current = scanner.current + 1 }
    else
      scanner
  in
  let scanner = loop scanner in
  let text = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
  let token_type = 
    match List.assoc_opt text keywords with
    | Some keyword -> keyword
    | None -> IDENTIFIER
  in
  (* Handle boolean literals specially *)
  let literal = 
    match token_type with
    | TRUE -> Some (Lit_bool true)
    | FALSE -> Some (Lit_bool false)
    | NIL -> Some Lit_nil
    | _ -> None
  in
  add_token scanner token_type literal

let scan_token scanner =
  let scanner = { scanner with start = scanner.current } in
  if is_at_end scanner then scanner
  else
    let (scanner, c) = advance scanner in
    match c with
    | '(' -> add_simple_token scanner LEFT_PAREN
    | ')' -> add_simple_token scanner RIGHT_PAREN
    | '{' -> add_simple_token scanner LEFT_BRACE
    | '}' -> add_simple_token scanner RIGHT_BRACE
    | ',' -> add_simple_token scanner COMMA
    | '.' -> add_simple_token scanner DOT
    | '-' -> add_simple_token scanner MINUS
    | '+' -> add_simple_token scanner PLUS
    | ';' -> add_simple_token scanner SEMICOLON
    | '*' -> add_simple_token scanner STAR
    | '!' -> 
        let (scanner, matched) = match_char scanner '=' in
        if matched then add_simple_token scanner BANG_EQUAL
        else add_simple_token scanner BANG
    | '=' ->
        let (scanner, matched) = match_char scanner '=' in
        if matched then add_simple_token scanner EQUAL_EQUAL
        else add_simple_token scanner EQUAL
    | '<' ->
        let (scanner, matched) = match_char scanner '=' in
        if matched then add_simple_token scanner LESS_EQUAL
        else add_simple_token scanner LESS
    | '>' ->
        let (scanner, matched) = match_char scanner '=' in
        if matched then add_simple_token scanner GREATER_EQUAL
        else add_simple_token scanner GREATER
    | '&' ->
        let (scanner, matched) = match_char scanner '&' in
        if matched then add_simple_token scanner AND
        else raise (ScanError ("Unexpected character: &", scanner.line))
    | '|' ->
        let (scanner, matched) = match_char scanner '|' in
        if matched then add_simple_token scanner OR
        else raise (ScanError ("Unexpected character: |", scanner.line))
    | '/' ->
        let (scanner, matched) = match_char scanner '/' in
        if matched then
          (* A comment goes until the end of the line *)
          let rec skip_comment scanner =
            if peek scanner <> '\n' && not (is_at_end scanner) then
              skip_comment { scanner with current = scanner.current + 1 }
            else
              scanner
          in
          skip_comment scanner
        else
          add_simple_token scanner SLASH
    | ' ' | '\r' | '\t' -> scanner (* Ignore whitespace *)
    | '\n' -> { scanner with line = scanner.line + 1 }
    | '"' -> scan_string scanner
    | c when is_digit c -> scan_number scanner
    | c when is_alpha c -> scan_identifier scanner
    | _ -> raise (ScanError ("Unexpected character: " ^ String.make 1 c, scanner.line))

let rec scan_tokens scanner =
  if is_at_end scanner then
    let eof_token = {
      token_type = EOF;
      lexeme = "";
      literal = None;
      line = scanner.line;
    } in
    List.rev (eof_token :: scanner.tokens)
  else
    let scanner = scan_token scanner in
    scan_tokens scanner

let scan source =
  try
    let scanner = create_scanner source in
    Ok (scan_tokens scanner)
  with
  | ScanError (msg, line) -> Error (Printf.sprintf "Line %d: %s" line msg)
