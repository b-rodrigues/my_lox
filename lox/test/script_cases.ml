open Printf

let cases_dir = "cases"
let interpreter = "../src/main.exe"

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let list_lox_files () =
  Sys.readdir cases_dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".lox")
  |> List.sort String.compare

let run_interpreter file =
  let cmd = Filename.quote_command interpreter [Filename.concat cases_dir file] in
  let ic, oc, ec = Unix.open_process_full cmd (Unix.environment ()) in
  close_out oc;
  let buf_out = Buffer.create 128 in
  let buf_err = Buffer.create 128 in
  (try while true do Buffer.add_string buf_out (input_line ic); Buffer.add_char buf_out '\n' done with End_of_file -> ());
  (try while true do Buffer.add_string buf_err (input_line ec); Buffer.add_char buf_err '\n' done with End_of_file -> ());
  let status = Unix.close_process_full (ic, oc, ec) in
  let code =
    match status with
    | Unix.WEXITED c -> c
    | Unix.WSIGNALED s -> 128 + s
    | Unix.WSTOPPED s -> 128 + s
  in
  code, Buffer.contents buf_out, Buffer.contents buf_err

let trim_newline s =
  if s <> "" && s.[String.length s - 1] = '\n'
  then String.sub s 0 (String.length s - 1)
  else s

let () =
  let files = list_lox_files () in
  let total = ref 0 and pass = ref 0 and fail = ref 0 and skip = ref 0 in
  List.iter (fun f ->
    incr total;
    let base = Filename.concat cases_dir (Filename.remove_extension f) in
    let out_file = base ^ ".out" in
    let err_file = base ^ ".err" in
    let has_out = Sys.file_exists out_file in
    let has_err = Sys.file_exists err_file in
    if has_out && has_err then (
      printf "[SKIP] %s (both .out and .err present)\n%!" f;
      incr skip
    ) else if (not has_out) && (not has_err) then (
      printf "[SKIP] %s (no expectation file)\n%!" f;
      incr skip
    ) else
      let code, stdout, stderr = run_interpreter f in
      if has_out then (
        let expected = read_file out_file in
        let stdout_norm = trim_newline stdout
        and expected_norm = trim_newline expected in
        if code <> 0 then (
          printf "[FAIL] %s (exit %d expected 0)\n" f code;
          if stderr <> "" then printf "  stderr: %S\n" stderr;
          incr fail
        ) else if stdout_norm <> expected_norm then (
          printf "[FAIL] %s output mismatch\n  expected: %S\n  got: %S\n" f expected stdout;
          incr fail
        ) else if stderr <> "" then (
          printf "[FAIL] %s unexpected stderr %S\n" f stderr;
          incr fail
        ) else (
          printf "[PASS] %s\n%!" f;
          incr pass
        )
      ) else if has_err then (
        let needed =
          read_file err_file
          |> String.split_on_char '\n'
          |> List.filter (fun s -> s <> "")
        in
        if code = 0 then (
          printf "[FAIL] %s expected non-zero exit\n  output: %S\n  stderr: %S\n"
            f stdout stderr;
          incr fail
        ) else
          let combined = stdout ^ stderr in
          let missing =
            List.filter (fun needle ->
              not (String.contains combined needle.[0]) ||
              (* full substring search *)
              let len_c = String.length combined
              and len_n = String.length needle in
              let rec search i =
                if len_n = 0 then true
                else if i + len_n > len_c then false
                else if String.sub combined i len_n = needle then true
                else search (i + 1)
              in
              not (search 0)
            ) needed
          in
          if missing = [] then (
            printf "[PASS] %s\n%!" f;
            incr pass
          ) else (
            printf "[FAIL] %s missing: %s\n%!"
              f (String.concat ", " missing);
            incr fail
          )
      )
  ) files;
  printf "Script Tests Summary: total=%d pass=%d fail=%d skip=%d\n%!"
    !total !pass !fail !skip;
  if !fail > 0 then exit 1
