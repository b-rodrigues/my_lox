open Lox

module T = struct
  type result = {
    name : string;
    passed : bool;
    log : string;
  }

  let tests : result list ref = ref []

  let record name passed log =
    tests := { name; passed; log } :: !tests

  let run name f =
    try
      f ();
      record name true ""
    with
    | Failure msg -> record name false msg
    | exn -> record name false (Printexc.to_string exn)

  let summarize () =
    let all = List.rev !tests in
    let failed = List.filter (fun t -> not t.passed) all in
    List.iter
      (fun t ->
         if t.passed then
           Printf.printf "[PASS] %s\n" t.name
         else
           Printf.printf "[FAIL] %s\n    %s\n" t.name t.log)
      all;
    Printf.printf "----\nTotal: %d  Passed: %d  Failed: %d\n%!"
      (List.length all)
      (List.length all - List.length failed)
      (List.length failed);
    if failed <> [] then exit 1
end

(* Helpers *)

let assert_bool msg b =
  if not b then failwith msg

let assert_eq ?(cmp = (=)) ?(pp = fun _ -> "<value>") expected actual =
  if not (cmp expected actual) then
    failwith (Printf.sprintf "Expected: %s\nActual:   %s"
                (pp expected) (pp actual))

let string_contains ~sub s =
  try
    let len_s = String.length s
    and len_sub = String.length sub in
    let rec loop i =
      if i + len_sub > len_s then false
      else if String.sub s i len_sub = sub then true
      else loop (i + 1)
    in
    loop 0
  with _ -> false

let capture_stdout f =
  let old_out = Unix.dup Unix.stdout in
  let (r, w) = Unix.pipe () in
  Unix.dup2 w Unix.stdout;
  Unix.close w;
  let buf = Buffer.create 128 in
  (try f () with e ->
     Unix.dup2 old_out Unix.stdout;
     Unix.close old_out;
     raise e);
  Unix.dup2 old_out Unix.stdout;
  Unix.close old_out;
  let ic = Unix.in_channel_of_descr r in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  close_in ic;
  Buffer.contents buf

let scan_exn src =
  match Scanner.scan src with
  | Ok toks -> toks
  | Error msg -> failwith ("Scan error: " ^ msg)

let token_type_to_string t =
  Token.(
    match t.token_type with
    | LEFT_PAREN -> "LEFT_PAREN"
    | RIGHT_PAREN -> "RIGHT_PAREN"
    | LEFT_BRACE -> "LEFT_BRACE"
    | RIGHT_BRACE -> "RIGHT_BRACE"
    | COMMA -> "COMMA"
    | DOT -> "DOT"
    | MINUS -> "MINUS"
    | PLUS -> "PLUS"
    | SEMICOLON -> "SEMICOLON"
    | SLASH -> "SLASH"
    | STAR -> "STAR"
    | BANG -> "BANG"
    | BANG_EQUAL -> "BANG_EQUAL"
    | EQUAL -> "EQUAL"
    | EQUAL_EQUAL -> "EQUAL_EQUAL"
    | GREATER -> "GREATER"
    | GREATER_EQUAL -> "GREATER_EQUAL"
    | LESS -> "LESS"
    | LESS_EQUAL -> "LESS_EQUAL"
    | IDENTIFIER -> "IDENTIFIER"
    | STRING -> "STRING"
    | BACKSLASH -> "BACKSLASH"
    | NUMBER -> "NUMBER"
    | AND -> "AND"
    | CLASS -> "CLASS"
    | ELSE -> "ELSE"
    | FALSE -> "FALSE"
    | FUN -> "FUN"
    | FOR -> "FOR"
    | IF -> "IF"
    | NIL -> "NIL"
    | OR -> "OR"
    | PRINT -> "PRINT"
    | RETURN -> "RETURN"
    | SUPER -> "SUPER"
    | THIS -> "THIS"
    | TRUE -> "TRUE"
    | VAR -> "VAR"
    | WHILE -> "WHILE"
    | EOF -> "EOF"
  )
  (* NOTE: If you add a new token constructor, this function will now
     trigger a non-exhaustive pattern warning again—good for visibility. *)

let take n lst =
  let rec aux i acc = function
    | _ when i = n -> List.rev acc
    | [] -> List.rev acc
    | x::xs -> aux (i+1) (x::acc) xs
  in
  aux 0 [] lst

let () =
  (* Test 1: Scanner basic expression *)
  T.run "scanner: basic expression tokens" (fun () ->
      let tokens = scan_exn "1 + 2 * (3 + 4)" in
      let types = List.map token_type_to_string tokens in
      let prefix = take 6 types in
      assert_eq
        ["NUMBER"; "PLUS"; "NUMBER"; "STAR"; "LEFT_PAREN"; "NUMBER"]
        prefix
        ~pp:(fun l -> String.concat ", " l)
    );

  (* Test 2: Parser precedence "1 + 2 * 3;" *)
  T.run "parser: precedence" (fun () ->
      let tokens = scan_exn "1 + 2 * 3;" in
      match Parser.parse tokens with
      | Error msg -> failwith ("Unexpected parse error: " ^ msg)
      | Ok [Ast.Expression expr] ->
          let rec shape = function
            | Ast.Binary (l, tok, r) ->
                let op =
                  Token.(match tok.token_type with
                  | PLUS -> "+"
                  | STAR -> "*"
                  | _ -> "?")
                in
                "(" ^ shape l ^ " " ^ op ^ " " ^ shape r ^ ")"
            | Ast.Literal (Token.Lit_number n) ->
                if Float.is_integer n then string_of_int (int_of_float n)
                else string_of_float n
            | _ -> "?"
          in
          assert_eq "(1 + (2 * 3))" (shape expr)
      | Ok _ -> failwith "Unexpected parsed statement list shape"
    );

  (* Helper to run snippet end-to-end *)
  let run snippet =
    let tokens = scan_exn snippet in
    match Parser.parse tokens with
    | Error msg -> Error ("Parse: " ^ msg)
    | Ok stmts ->
        (match Resolver.resolve stmts with
         | Error msg -> Error ("Resolve: " ^ msg)
         | Ok r ->
             (match Interpreter.interpret r with
              | Ok () -> Ok ()
              | Error msg -> Error ("Runtime: " ^ msg)))
  in

  (* Test 3: Execution print *)
  T.run "interp: arithmetic print" (fun () ->
      let output =
        capture_stdout (fun () ->
            match run "print 6 * (2 + 1);" with
            | Ok () -> ()
            | Error e -> failwith e)
      in
      assert_eq "18\n" output ~pp:(fun s -> s)
    );

  (* Test 4: Resolver error 'this' outside class *)
  T.run "resolver: 'this' outside class error" (fun () ->
      let tokens = scan_exn "print this;" in
      match Parser.parse tokens with
      | Error msg -> failwith ("Parse unexpectedly failed: " ^ msg)
      | Ok stmts ->
          (match Resolver.resolve stmts with
           | Ok _ -> failwith "Expected resolve error using 'this' outside class"
           | Error msg ->
               assert_bool ("No 'this' in message: " ^ msg)
                 (string_contains ~sub:"this" msg))
    );

  (* Test 5: super misuse *)
  T.run "resolver: 'super' no superclass error" (fun () ->
      let code = "class A { method() { super.m(); } }" in
      let tokens = scan_exn code in
      match Parser.parse tokens with
      | Error msg -> failwith ("Parse error: " ^ msg)
      | Ok stmts ->
          (match Resolver.resolve stmts with
           | Ok _ -> failwith "Expected resolver error for super with no superclass"
           | Error msg ->
               assert_bool ("Message missing 'super': "^msg)
                 (string_contains ~sub:"super" msg))
    );

  T.summarize ()
