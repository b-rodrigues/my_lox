open Token

let keywords =
  let open Hashtbl in
  let tbl = create 32 in
  List.iter (fun (k, v) -> replace tbl k v)
    [ ("and", AND); ("class", CLASS); ("else", ELSE); ("false", FALSE);
      ("for", FOR); ("fun", FUN); ("if", IF); ("nil", NIL); ("or", OR);
      ("print", PRINT); ("return", RETURN); ("super", SUPER); ("this", THIS);
      ("true", TRUE); ("var", VAR); ("while", WHILE) ];
  tbl

type scanner = {
  src : string;
  mutable start : int;
  mutable current : int;
  mutable line : int;
  tokens : token list ref;
}

let create source =
  { src = source; start = 0; current = 0; line = 1; tokens = ref [] }

let is_at_end s = s.current >= String.length s.src

let advance s =
  let ch = s.src.[s.current] in
  s.current <- s.current + 1;
  ch

(* Simpler: pass the literal explicitly, avoid optional-arg typing pitfalls *)
let add_token s token_type lit_opt =
  let text = String.sub s.src s.start (s.current - s.start) in
  let tok = { token_type; lexeme = text; literal = lit_opt; line = s.line } in
  s.tokens := tok :: !(s.tokens)

let match_char s expected =
  if is_at_end s then false
  else if s.src.[s.current] <> expected then false
  else (s.current <- s.current + 1; true)

let peek s =
  if is_at_end s then '\000' else s.src.[s.current]

let peek_next s =
  if s.current + 1 >= String.length s.src then '\000'
  else s.src.[s.current + 1]

let string s =
  while not (is_at_end s) && peek s <> '"' do
    if peek s = '\n' then s.line <- s.line + 1;
    ignore (advance s)
  done;
  if is_at_end s then Error (Printf.sprintf "Line %d: Unterminated string." s.line)
  else (
    ignore (advance s); (* closing quote *)
    let value = String.sub s.src (s.start + 1) (s.current - s.start - 2) in
    add_token s STRING (Some (Lit_string value));
    Ok ()
  )

let is_digit c = c >= '0' && c <= '9'
let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_alpha c || is_digit c

let number s =
  while is_digit (peek s) do ignore (advance s) done;
  if peek s = '.' && is_digit (peek_next s) then (
    ignore (advance s);
    while is_digit (peek s) do ignore (advance s) done
  );
  let text = String.sub s.src s.start (s.current - s.start) in
  let f =
    try float_of_string text
    with _ -> nan
  in
  add_token s NUMBER (Some (Lit_number f))

let identifier s =
  while is_alnum (peek s) do ignore (advance s) done;
  let text = String.sub s.src s.start (s.current - s.start) in
  let tt =
    match Hashtbl.find_opt keywords text with
    | Some kw -> kw
    | None -> IDENTIFIER
  in
  let lit =
    match tt with
    | TRUE -> Some (Lit_bool true)
    | FALSE -> Some (Lit_bool false)
    | NIL -> Some Lit_nil
    | _ -> None
  in
  add_token s tt lit

let scan_token s =
  s.start <- s.current;
  let c = advance s in
  match c with
  | '(' -> add_token s LEFT_PAREN None; Ok ()
  | ')' -> add_token s RIGHT_PAREN None; Ok ()
  | '{' -> add_token s LEFT_BRACE None; Ok ()
  | '}' -> add_token s RIGHT_BRACE None; Ok ()
  | ',' -> add_token s COMMA None; Ok ()
  | '.' -> add_token s DOT None; Ok ()
  | '-' -> add_token s MINUS None; Ok ()
  | '+' -> add_token s PLUS None; Ok ()
  | ';' -> add_token s SEMICOLON None; Ok ()
  | '*' -> add_token s STAR None; Ok ()
  | '\\' -> add_token s BACKSLASH None; Ok ()
  | '!' -> add_token s (if match_char s '=' then BANG_EQUAL else BANG) None; Ok ()
  | '=' -> add_token s (if match_char s '=' then EQUAL_EQUAL else EQUAL) None; Ok ()
  | '<' -> add_token s (if match_char s '=' then LESS_EQUAL else LESS) None; Ok ()
  | '>' -> add_token s (if match_char s '=' then GREATER_EQUAL else GREATER) None; Ok ()
  | '/' ->
      if match_char s '/' then (
        while (peek s <> '\n') && not (is_at_end s) do ignore (advance s) done;
        Ok ()
      ) else (
        add_token s SLASH None; Ok ()
      )
  | ' ' | '\r' | '\t' -> Ok ()
  | '\n' -> s.line <- s.line + 1; Ok ()
  | '"' -> string s
  | c when is_digit c -> number s; Ok ()
  | c when is_alpha c -> identifier s; Ok ()
  | _ ->
      Error (Printf.sprintf "Line %d: Unexpected character." s.line)

let scan source =
  let s = create source in
  let rec loop () =
    if is_at_end s then Ok ()
    else
      match scan_token s with
      | Ok () -> loop ()
      | Error e -> Error e
  in
  match loop () with
  | Error e -> Error e
  | Ok () ->
      s.start <- s.current;
      add_token s EOF None;
      Ok (List.rev !(s.tokens))
