open Lox

let run source is_repl =
  try
    match Scanner.scan source with
    | Ok tokens -> (
        match Parser.parse tokens with
        | Ok statements -> (
            (* REPL auto-print tweak first so resolver sees final form *)
            let statements =
              if is_repl then
                match statements with
                | [Ast.Expression expr] -> [Ast.Print expr]
                | _ -> statements
              else
                statements
            in
            (* Resolve *)
            match Resolver.resolve statements with
            | Error msg ->
                Printf.eprintf "Resolve error: %s\n" msg;
                flush stderr;
                false
            | Ok resolved ->
                match Interpreter.interpret resolved with
                | Ok () -> true
                | Error msg ->
                    Printf.eprintf "Runtime error: %s\n" msg;
                    flush stderr;
                    false
          )
        | Error msg ->
            Printf.eprintf "Parse error: %s\n" msg;
            flush stderr;
            false
      )
    | Error msg ->
        Printf.eprintf "Scan error: %s\n" msg;
        flush stderr;
        false
  with
  | Failure msg ->
      Printf.eprintf "Internal error: %s\n" msg;
      flush stderr;
      false

let run_file filename =
  let ic = open_in filename in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  if not (run source false) then exit 65

let run_prompt () =
  let read_until_balanced first_prompt cont_prompt =
    let buf = Buffer.create 256 in
    let rec loop depth prompt =
      match LNoise.linenoise prompt with
      | None -> None
      | Some line ->
          Buffer.add_string buf (line ^ "\n");
          let opens  = String.fold_left (fun acc c -> if c = '{' then acc + 1 else acc) 0 line in
          let closes = String.fold_left (fun acc c -> if c = '}' then acc + 1 else acc) 0 line in
          let depth = depth + opens - closes in
          if depth > 0 then
            loop depth cont_prompt
          else
            Some (Buffer.contents buf)
    in
    loop 0 first_prompt
  in

  let rec loop () =
    match read_until_balanced "> " "... " with
    | None -> print_endline ""; ()
    | Some source ->
        ignore (LNoise.history_add source);
        ignore (run source true);
        loop ()
  in
  loop ()

let () =
  Builtins.register_all Interpreter.global_env;
  match Sys.argv with
  | [| _ |] ->
      Printf.printf "Welcome to Lox REPL!\n";
      flush stdout;
      run_prompt ()
  | [| _; filename |] ->
      run_file filename
  | _ ->
      Printf.eprintf "Usage: %s [script]\n" Sys.argv.(0);
      exit 1
