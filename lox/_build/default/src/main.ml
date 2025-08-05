open Lox

let run source =
  match Scanner.scan source with
  | Ok tokens ->
      (match Parser.parse tokens with
       | Ok statements ->
           (match Interpreter.interpret statements with
            | Ok () -> ()
            | Error msg ->
                Printf.eprintf "Runtime error: %s\n" msg;
                exit 1)
       | Error msg ->
           Printf.eprintf "Parse error: %s\n" msg;
           exit 1)
  | Error msg ->
      Printf.eprintf "Scan error: %s\n" msg;
      exit 1

let run_file filename =
  let ic = open_in filename in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  run source

let run_prompt () =
  let rec loop () =
    Printf.printf "> ";
    flush stdout;
    match input_line stdin with
    | exception End_of_file -> Printf.printf "\n"
    | line ->
        run line;
        loop ()
  in
  loop ()

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
