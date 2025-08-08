open Interpreter

let define_native env name arity call =
  Environment.define env name (Val_native { name; arity; call })

let register_all env =
  (* clock(time = "unix" | "human") *)
  define_native env "clock" 0 (fun ~line pos named ->
    if pos <> [] then
      raise (RuntimeError ("clock() does not take positional arguments; use clock(time = \"unix\"|\"human\")", line));
    let mode =
      match List.find_opt (fun (k, _) -> k = "time") named with
      | None -> "unix"
      | Some (_, Val_string s) -> s
      | Some _ -> raise (RuntimeError ("Expected 'time' to be a string.", line))
    in
    match mode with
    | "unix" ->
        Val_number (Unix.gettimeofday ())
    | "human" ->
        let tm = Unix.localtime (Unix.time ()) in
        let year = tm.tm_year + 1900 in
        let mon  = tm.tm_mon + 1 in
        let day  = tm.tm_mday in
        let hour = tm.tm_hour in
        let min  = tm.tm_min in
        let sec  = tm.tm_sec in
        Val_string (Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
                      year mon day hour min sec)
    | other ->
        raise (RuntimeError (Printf.sprintf "Unknown time mode '%s'. Use \"unix\" or \"human\"." other, line))
  )
