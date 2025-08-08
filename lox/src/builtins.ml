open Interpreter

let define_native env name arity call =
  Environment.define env name (Val_native { name; arity; call })

let register_all env =
  (* clock(): seconds since the Unix epoch *)
  define_native env "clock" 0 (fun _ -> Val_number (Unix.gettimeofday ()))
