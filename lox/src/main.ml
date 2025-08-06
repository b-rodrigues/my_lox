open Lox

(*
 * MODIFIED: This function now returns a boolean indicating success (true) or failure (false).
 * It no longer calls `exit` directly.
 *)
let run source is_repl =
  try
    match Scanner.scan source with
    | Ok tokens -> (
        match Parser.parse tokens with
        | Ok statements -> (
            let statements =
              if is_repl then
                match statements with
                | [Ast.Expression expr] -> [Ast.Print expr]
                | _ -> statements
              else
                statements
            in
            (* Assuming an interpreter.ml file with a similar structure *)
            match Interpreter.interpret statements with
            | Ok () -> true (* Success *)
            | Error msg ->
                Printf.eprintf "Runtime error: %s\n" msg;
                flush stderr;
                false (* Failure *)
            )
        | Error msg ->
            Printf.eprintf "Parse error: %s\n" msg;
            flush stderr;
            false (* Failure *)
        )
    | Error msg ->
        Printf.eprintf "Scan error: %s\n" msg;
        flush stderr;
        false (* Failure *)
  with
  (* Add a catch-all for unexpected exceptions from modules if they aren't caught already *)
  | Failure msg ->
      Printf.eprintf "Internal error: %s\n" msg;
      flush stderr;
      false

(*
 * MODIFIED: This function now checks the boolean result of `run`
 * and is responsible for exiting if an error occurred.
 *)
let run_file filename =
  let ic = open_in filename in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  if not (run source false) then exit 65 (* Standard exit code for data error *)


(*
 * MODIFIED: The main loop now calls `run` but ignores its return value.
 * Whether it succeeds or fails, we want the loop to continue.
 *)
let run_prompt () =
  let rec loop () =
    Printf.printf "> ";
    flush stdout;
    match input_line stdin with
    | exception End_of_file -> Printf.printf "\n"
    | line ->
        let _ = run line true in (* We don't care about the result, just that it ran *)
        loop ()
  in
  loop ()

(* This part remains the same *)
let () =
  match Sys.argv with
  | [| _ |] ->
      Printf.printf "Welcome to Lox REPL!\n";
      run_prompt ()
  | [| _; filename |] ->
      run_file filename
  | _ ->
      Printf.eprintf "Usage: %s [script]\n" Sys.argv.(0);
      exit 1
