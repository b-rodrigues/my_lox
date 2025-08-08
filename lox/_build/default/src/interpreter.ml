open Token
open Ast

type env = {
  values    : (string, value) Hashtbl.t;
  enclosing : env option;
}
and value =
  | Val_number of float
  | Val_string of string
  | Val_bool   of bool
  | Val_nil
  | Val_function of lox_function
  | Val_native of native_fn
and lox_function = {
  name    : string;
  params  : string list;
  body    : stmt list;
  closure : env;
}
and native_fn = {
  name  : string;
  arity : int;  (* positional args required *)
  call  : line:int -> value list -> (string * value) list -> value;
}

exception RuntimeError of string * int
exception Return of value

module Environment = struct
  type t = env

  let create ?enclosing () : t =
    { values = Hashtbl.create 16; enclosing }

  let rec get (env : t) (name : string) : value =
    match Hashtbl.find_opt env.values name with
    | Some v -> v
    | None ->
        (match env.enclosing with
         | Some parent -> get parent name
         | None -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1)))

  let define (env : t) (name : string) (v : value) : unit =
    Hashtbl.replace env.values name v

  let rec assign (env : t) (name : string) (v : value) : unit =
    if Hashtbl.mem env.values name then
      Hashtbl.replace env.values name v
    else
      match env.enclosing with
      | Some parent -> assign parent name v
      | None -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1))

  let rec ancestor (env : t) distance : t =
    if distance = 0 then env
    else match env.enclosing with
      | Some e -> ancestor e (distance - 1)
      | None -> env

  let get_at (env : t) distance name : value =
    let target = ancestor env distance in
    match Hashtbl.find_opt target.values name with
    | Some v -> v
    | None -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1))

  let assign_at (env : t) distance name v : unit =
    let target = ancestor env distance in
    if Hashtbl.mem target.values name then
      Hashtbl.replace target.values name v
    else
      raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1))
end

let literal_to_value = function
  | Lit_number n -> Val_number n
  | Lit_string s -> Val_string s
  | Lit_bool b   -> Val_bool b
  | Lit_nil      -> Val_nil

let value_to_string = function
  | Val_number n ->
      if n = Float.round n then Printf.sprintf "%.0f" n
      else string_of_float n
  | Val_string s  -> s
  | Val_bool true -> "true"
  | Val_bool false-> "false"
  | Val_nil       -> "nil"
  | Val_function fn -> Printf.sprintf "<fn %s>" fn.name
  | Val_native nf   -> Printf.sprintf "<native fn %s>" nf.name

let is_truthy = function
  | Val_bool false | Val_nil -> false
  | _ -> true

let is_equal v1 v2 =
  match (v1, v2) with
  | (Val_nil, Val_nil) -> true
  | (Val_nil, _) | (_, Val_nil) -> false
  | (Val_bool b1, Val_bool b2) -> b1 = b2
  | (Val_number x, Val_number y) -> x = y
  | (Val_string s1, Val_string s2) -> s1 = s2
  | _ -> false

let rec evaluate env = function
  | Assign (name, expr, depth_opt, _line) ->
      let value = evaluate env expr in
      (match depth_opt with
       | Some d -> Environment.assign_at env d name value
       | None -> Environment.assign env name value);
      value

  | Literal lit ->
      literal_to_value lit

  | Variable (name, depth_opt, _line) ->
      (match depth_opt with
       | Some d -> Environment.get_at env d name
       | None -> Environment.get env name)

  | Grouping expr ->
      evaluate env expr

  | Unary (operator, right) ->
      let rv = evaluate env right in
      (match operator.token_type with
       | MINUS ->
           (match rv with
            | Val_number n -> Val_number (-.n)
            | _ -> raise (RuntimeError ("Operand must be a number.", operator.line)))
       | BANG ->
           Val_bool (not (is_truthy rv))
       | _ ->
           raise (RuntimeError ("Unknown unary operator.", operator.line))
      )

  | Binary (left, operator, right) ->
      (match operator.token_type with
       | OR ->
           let lv = evaluate env left in
           if is_truthy lv then lv else evaluate env right
       | AND ->
           let lv = evaluate env left in
           if not (is_truthy lv) then lv else evaluate env right
       | _ ->
           let lv = evaluate env left in
           let rv = evaluate env right in
           match operator.token_type with
           | PLUS ->
               (match (lv, rv) with
                | (Val_number a, Val_number b) -> Val_number (a +. b)
                | (Val_string a, Val_string b) -> Val_string (a ^ b)
                | _ -> raise (RuntimeError ("Operands must be two numbers or two strings.", operator.line)))
           | MINUS ->
               (match (lv, rv) with
                | (Val_number a, Val_number b) -> Val_number (a -. b)
                | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
           | STAR ->
               (match (lv, rv) with
                | (Val_number a, Val_number b) -> Val_number (a *. b)
                | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
           | SLASH ->
               (match (lv, rv) with
                | (Val_number _, Val_number 0.) -> raise (RuntimeError ("Division by zero.", operator.line))
                | (Val_number a, Val_number b) -> Val_number (a /. b)
                | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
           | GREATER ->
               (match (lv, rv) with
                | (Val_number a, Val_number b) -> Val_bool (a > b)
                | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
           | GREATER_EQUAL ->
               (match (lv, rv) with
                | (Val_number a, Val_number b) -> Val_bool (a >= b)
                | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
           | LESS ->
               (match (lv, rv) with
                | (Val_number a, Val_number b) -> Val_bool (a < b)
                | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
           | LESS_EQUAL ->
               (match (lv, rv) with
                | (Val_number a, Val_number b) -> Val_bool (a <= b)
                | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
           | EQUAL_EQUAL ->
               Val_bool (is_equal lv rv)
           | BANG_EQUAL ->
               Val_bool (not (is_equal lv rv))
           | _ ->
               raise (RuntimeError ("Unknown binary operator.", operator.line))
      )

  | Call (callee_expr, paren, args) ->
      let callee = evaluate env callee_expr in
      let eval_args args =
        let rec loop args pos_acc named_acc seen_named =
          match args with
          | [] -> (List.rev pos_acc, List.rev named_acc)
          | Arg_pos e :: rest ->
              if seen_named then
                raise (RuntimeError ("Positional arguments cannot follow named arguments.", paren.line));
              let v = evaluate env e in
              loop rest (v :: pos_acc) named_acc seen_named
          | Arg_named (name, e, _line) :: rest ->
              let v = evaluate env e in
              if List.exists (fun (n, _) -> n = name) named_acc then
                raise (RuntimeError (Printf.sprintf "Duplicate named argument '%s'." name, paren.line));
              loop rest pos_acc ((name, v) :: named_acc) true
        in
        loop args [] [] false
      in
      (match callee with
       | Val_function fn ->
           let (pos_args, named_args) = eval_args args in
           let param_count = List.length fn.params in
           let pos_count = List.length pos_args in
           let named_count = List.length named_args in
           if pos_count > param_count then
             raise (RuntimeError (Printf.sprintf "Expected at most %d positional arguments but got %d." param_count pos_count, paren.line));

           let idx_of =
             let tbl = Hashtbl.create 16 in
             List.iteri (fun i p -> Hashtbl.replace tbl p i) fn.params;
             fun name -> Hashtbl.find_opt tbl name
           in

           List.iter
             (fun (name, _v) ->
               match idx_of name with
               | None ->
                   raise (RuntimeError (Printf.sprintf "Unknown parameter '%s'." name, paren.line))
               | Some i ->
                   if i < pos_count then
                     raise (RuntimeError (Printf.sprintf "Parameter '%s' already supplied by position." name, paren.line)))
             named_args;

           if pos_count + named_count <> param_count then
             raise (RuntimeError (Printf.sprintf "Expected %d arguments but got %d." param_count (pos_count + named_count), paren.line));

           let call_env = Environment.create ~enclosing:fn.closure () in
           (* Bind positional *)
           List.iteri
             (fun i v ->
               let param = List.nth fn.params i in
               Environment.define call_env param v)
             pos_args;
           (* Bind named *)
           List.iter
             (fun (name, v) -> Environment.define call_env name v)
             named_args;

           (try
              List.iter (execute call_env) fn.body;
              Val_nil
            with
            | Return v -> v)

       | Val_native nf ->
           let (pos_args, named_args) = eval_args args in
           let got = List.length pos_args in
           if got <> nf.arity then
             raise (RuntimeError (Printf.sprintf "Expected %d positional arguments but got %d." nf.arity got, paren.line));
           nf.call ~line:paren.line pos_args named_args

       | _ ->
           raise (RuntimeError ("Can only call functions.", paren.line))
      )

and execute env = function
  | Expression expr ->
      let _ = evaluate env expr in
      ()

  | Print expr ->
      let v = evaluate env expr in
      Printf.printf "%s\n" (value_to_string v);
      flush stdout

  | Var (name, init_opt) ->
      let v = match init_opt with Some e -> evaluate env e | None -> Val_nil in
      Environment.define env name v

  | If (cond, then_b, else_b) ->
      if is_truthy (evaluate env cond) then
        execute env then_b
      else
        (match else_b with Some stmt -> execute env stmt | None -> ())

  | Block stmts ->
      let local = Environment.create ~enclosing:env () in
      List.iter (execute local) stmts

  | While (cond, body) ->
      while is_truthy (evaluate env cond) do
        execute env body
      done

  | Fun (name, params, body) ->
      let fn = {
        name;
        params;
        body;
        closure = env;
      } in
      Environment.define env name (Val_function fn)

  | Return (_tok, expr_opt) ->
      let v = match expr_opt with Some e -> evaluate env e | None -> Val_nil in
      raise (Return v)

let global_env = Environment.create ()

let interpret statements =
  try
    List.iter (execute global_env) statements;
    Ok ()
  with
  | RuntimeError (msg, line) ->
      Error (Printf.sprintf "Line %d: %s" line msg)
  | Return _ ->
      Error ("Return statement outside of function.")
