open Token
open Ast

type env = {
  values    : (string, value) Hashtbl.t;
  enclosing : env option;
}

and lox_function = {
  name        : string;
  params      : string list;
  body        : stmt list;
  closure     : env;               (* For methods: this is the methods_env (holds super if subclass). *)
  bound_this  : value option;      (* Some (Val_instance ...) when method is bound, else None. *)
}

and native_fn = {
  name  : string;
  arity : int;
  call  : line:int -> value list -> (string * value) list -> value;
}

and class_value = {
  cname   : string;
  methods : (string, lox_function) Hashtbl.t;
  super   : class_value option;
}

and instance = {
  klass  : class_value;
  fields : (string, value) Hashtbl.t;
}

and value =
  | Val_number of float
  | Val_string of string
  | Val_bool   of bool
  | Val_nil
  | Val_function of lox_function
  | Val_native of native_fn
  | Val_class of class_value
  | Val_instance of instance

exception RuntimeError of string * int
exception Return of value

module Environment = struct
  type t = env

  let create ?enclosing () =
    { values = Hashtbl.create 16; enclosing }

  let rec get env name =
    match Hashtbl.find_opt env.values name with
    | Some v -> v
    | None ->
        (match env.enclosing with
         | Some parent -> get parent name
         | None -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1)))

  let define env name v =
    Hashtbl.replace env.values name v

  let rec assign env name v =
    if Hashtbl.mem env.values name then
      Hashtbl.replace env.values name v
    else
      match env.enclosing with
      | Some parent -> assign parent name v
      | None -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1))

  let rec ancestor env distance =
    if distance = 0 then env
    else match env.enclosing with
      | Some e -> ancestor e (distance - 1)
      | None -> env

  let get_at env distance name =
    let target = ancestor env distance in
    match Hashtbl.find_opt target.values name with
    | Some v -> v
    | None -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1))

  let assign_at env distance name v =
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
      if n = Float.round n then Printf.sprintf "%.0f" n else string_of_float n
  | Val_string s -> s
  | Val_bool true -> "true"
  | Val_bool false -> "false"
  | Val_nil -> "nil"
  | Val_function fn -> Printf.sprintf "<fn %s>" fn.name
  | Val_native nf -> Printf.sprintf "<native fn %s>" nf.name
  | Val_class c -> Printf.sprintf "<class %s>" c.cname
  | Val_instance inst -> Printf.sprintf "<instance %s>" inst.klass.cname

let is_truthy = function
  | Val_bool false | Val_nil -> false
  | _ -> true

let is_equal v1 v2 =
  match v1, v2 with
  | Val_nil, Val_nil -> true
  | Val_nil, _ | _, Val_nil -> false
  | Val_bool a, Val_bool b -> a = b
  | Val_number a, Val_number b -> a = b
  | Val_string a, Val_string b -> a = b
  | _ -> false

(* Bind method without inserting an extra environment layer; we just store the instance. *)
let bind_method (inst : instance) (fn : lox_function) : value =
  Val_function { fn with bound_this = Some (Val_instance inst) }

let rec find_method klass name =
  match Hashtbl.find_opt klass.methods name with
  | Some fn -> Some fn
  | None ->
      (match klass.super with
       | Some super -> find_method super name
       | None -> None)

(* Evaluation *)

let rec evaluate env = function
  | Assign (name, expr, depth_opt, _) ->
      let value = evaluate env expr in
      (match depth_opt with
       | Some d -> Environment.assign_at env d name value
       | None -> Environment.assign env name value);
      value
  | Literal lit -> literal_to_value lit
  | Variable (name, depth_opt, _) ->
      (match depth_opt with
       | Some d -> Environment.get_at env d name
       | None -> Environment.get env name)
  | Grouping e -> evaluate env e
  | Unary (op, right) ->
      let rv = evaluate env right in
      (match op.token_type, rv with
       | MINUS, Val_number n -> Val_number (-.n)
       | MINUS, _ -> raise (RuntimeError ("Operand must be a number.", op.line))
       | BANG, _ -> Val_bool (not (is_truthy rv))
       | _ -> raise (RuntimeError ("Unknown unary operator.", op.line)))
  | Binary (l, op, r) ->
      (match op.token_type with
       | OR ->
           let lv = evaluate env l in
           if is_truthy lv then lv else evaluate env r
       | AND ->
           let lv = evaluate env l in
           if not (is_truthy lv) then lv else evaluate env r
       | _ ->
           let lv = evaluate env l in
           let rv = evaluate env r in
           (match op.token_type with
            | PLUS ->
                (match lv, rv with
                 | Val_number a, Val_number b -> Val_number (a +. b)
                 | Val_string a, Val_string b -> Val_string (a ^ b)
                 | Val_string a, _ -> Val_string (a ^ value_to_string rv)
                 | _, Val_string b -> Val_string (value_to_string lv ^ b)
                 | _ ->
                     raise (RuntimeError ("Operands must be two numbers or involve at least one string.", op.line)))
            | MINUS ->
                (match lv, rv with
                 | Val_number a, Val_number b -> Val_number (a -. b)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", op.line)))
            | STAR ->
                (match lv, rv with
                 | Val_number a, Val_number b -> Val_number (a *. b)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", op.line)))
            | SLASH ->
                (match lv, rv with
                 | Val_number _, Val_number 0. ->
                     raise (RuntimeError ("Division by zero.", op.line))
                 | Val_number a, Val_number b -> Val_number (a /. b)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", op.line)))
            | GREATER ->
                (match lv, rv with
                 | Val_number a, Val_number b -> Val_bool (a > b)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", op.line)))
            | GREATER_EQUAL ->
                (match lv, rv with
                 | Val_number a, Val_number b -> Val_bool (a >= b)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", op.line)))
            | LESS ->
                (match lv, rv with
                 | Val_number a, Val_number b -> Val_bool (a < b)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", op.line)))
            | LESS_EQUAL ->
                (match lv, rv with
                 | Val_number a, Val_number b -> Val_bool (a <= b)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", op.line)))
            | EQUAL_EQUAL -> Val_bool (is_equal lv rv)
            | BANG_EQUAL  -> Val_bool (not (is_equal lv rv))
            | _ -> raise (RuntimeError ("Unknown binary operator.", op.line))))
  | Get (obj_e, name, line) ->
      let obj = evaluate env obj_e in
      (match obj with
       | Val_instance inst ->
           (match Hashtbl.find_opt inst.fields name with
            | Some v -> v
            | None ->
                (match find_method inst.klass name with
                 | Some fn -> bind_method inst fn
                 | None -> raise (RuntimeError (Printf.sprintf "Undefined property '%s'." name, line))))
       | _ ->
           raise (RuntimeError ("Only instances have properties.", line)))
  | Set (obj_e, name, val_e, line) ->
      let obj = evaluate env obj_e in
      (match obj with
       | Val_instance inst ->
           let v = evaluate env val_e in
           Hashtbl.replace inst.fields name v;
           v
       | _ -> raise (RuntimeError ("Only instances have fields.", line)))
  | Super (_tok, method_name, line, depth_opt) ->
      (* depth points to scope containing 'super' *)
      let super_value =
        match depth_opt with
        | Some d -> Environment.get_at env d "super"
        | None -> Environment.get env "super"
      in
      let super_class =
        match super_value with
        | Val_class c -> c
        | _ -> raise (RuntimeError ("'super' is not a class.", line))
      in
      (* 'this' is at depth 1 (params at 0, 'this' at 1, super at 2) *)
      let this_val = Environment.get env "this" in
      let inst =
        match this_val with
        | Val_instance i -> i
        | _ -> raise (RuntimeError ("'this' is not an instance.", line))
      in
      (match find_method super_class method_name with
       | None ->
           raise (RuntimeError (Printf.sprintf "Undefined superclass method '%s'." method_name, line))
       | Some fn -> bind_method inst fn)
  | Lambda (params, body, _line) ->
      Val_function { name = "lambda"; params; body; closure = env; bound_this = None }
  | Call (callee_e, paren, args) ->
      let callee = evaluate env callee_e in
      let rec collect_args lst pos named seen_named =
        match lst with
        | [] -> List.rev pos, List.rev named
        | Arg_pos e :: rest ->
            if seen_named then
              raise (RuntimeError ("Positional arguments cannot follow named arguments.", paren.line));
            collect_args rest (evaluate env e :: pos) named seen_named
        | Arg_named (n, e, _l) :: rest ->
            let v = evaluate env e in
            if List.exists (fun (n2, _) -> n2 = n) named then
              raise (RuntimeError (Printf.sprintf "Duplicate named argument '%s'." n, paren.line));
            collect_args rest pos ((n, v) :: named) true
      in
      let pos_args, named_args = collect_args args [] [] false in

      let apply_function fn pos_args named_args =
        let param_count = List.length fn.params in
        let pos_count = List.length pos_args in
        let named_count = List.length named_args in
        if pos_count > param_count then
          raise (RuntimeError (Printf.sprintf "Expected at most %d positional arguments but got %d."
                                 param_count pos_count, paren.line));
        let idx_tbl = Hashtbl.create 16 in
        List.iteri (fun i p -> Hashtbl.add idx_tbl p i) fn.params;
        List.iter
          (fun (name, _v) ->
             match Hashtbl.find_opt idx_tbl name with
             | None ->
                 raise (RuntimeError (Printf.sprintf "Unknown parameter '%s'." name, paren.line))
             | Some i ->
                 if i < pos_count then
                   raise (RuntimeError (Printf.sprintf "Parameter '%s' already supplied by position." name, paren.line)))
          named_args;
        if pos_count + named_count <> param_count then
          raise (RuntimeError (Printf.sprintf "Expected %d arguments but got %d."
                                 param_count (pos_count + named_count), paren.line));
        (* Runtime layering for methods (matching resolver):
             params_env (depth 0)
             this_env   (depth 1)   [only if bound_this present]
             methods_env (fn.closure) (holds super at depth 2 for subclass)
           For plain functions (no bound_this): only params_env -> fn.closure *)
        let base_enclosing =
          match fn.bound_this with
          | Some thisv ->
              let this_env = Environment.create ~enclosing:fn.closure () in
              Environment.define this_env "this" thisv;
              this_env   (* this_env is depth1; we'll add params_env above it *)
          | None -> fn.closure
        in
        let params_env = Environment.create ~enclosing:base_enclosing () in
        (* define params (positional first) *)
        List.iteri
          (fun i v ->
             let p = List.nth fn.params i in
             Environment.define params_env p v)
          pos_args;
        List.iter (fun (n, v) -> Environment.define params_env n v) named_args;
        try
          List.iter (execute params_env) fn.body;
          Val_nil
        with
        | Return v -> v
      in
      (match callee with
       | Val_function fn ->
           apply_function fn pos_args named_args
       | Val_native nf ->
           let got = List.length pos_args in
           if got <> nf.arity then
             raise (RuntimeError (Printf.sprintf "Expected %d positional arguments but got %d."
                                    nf.arity got, paren.line));
           nf.call ~line:paren.line pos_args named_args
       | Val_class klass ->
           let inst = { klass; fields = Hashtbl.create 16 } in
           (match find_method klass "init" with
            | Some init_fn ->
                ignore (apply_function
                          (match bind_method inst init_fn with
                           | Val_function f -> f
                           | _ -> init_fn)
                          pos_args named_args)
            | None ->
                if pos_args <> [] || named_args <> [] then
                  raise (RuntimeError ("Class has no initializer.", paren.line)));
           Val_instance inst
       | _ ->
           raise (RuntimeError ("Can only call functions or classes.", paren.line)))

and execute env = function
  | Expression e -> ignore (evaluate env e)
  | Print e ->
    let v = evaluate env e in
    Printf.printf "%s\n" (value_to_string v); flush stdout
  | Var (name, init_opt) ->
      let v = match init_opt with Some e -> evaluate env e | None -> Val_nil in
      Environment.define env name v
  | If (cond, then_s, else_s) ->
      if is_truthy (evaluate env cond) then execute env then_s
      else (match else_s with Some s -> execute env s | None -> ())
  | Block stmts ->
      let local = Environment.create ~enclosing:env () in
      List.iter (execute local) stmts
  | While (cond, body) ->
      while is_truthy (evaluate env cond) do execute env body done
  | Fun (name, params, body) ->
      let fn = { name; params; body; closure = env; bound_this = None } in
      Environment.define env name (Val_function fn)
  | Class (name, superclass_opt, methods) ->
      (* Evaluate superclass first *)
      let superclass_val =
        match superclass_opt with
        | None -> None
        | Some expr ->
            (match evaluate env expr with
             | Val_class c -> Some c
             | _ -> raise (RuntimeError ("Superclass must be a class.", 1)))
      in
      (* methods_env holds 'super' if subclass *)
      let methods_env =
        match superclass_val with
        | Some super_cls ->
            let e = Environment.create ~enclosing:env () in
            Environment.define e "super" (Val_class super_cls);
            e
        | None -> env
      in
      let method_table = Hashtbl.create 16 in
      let klass = { cname = name; methods = method_table; super = superclass_val } in
      Environment.define env name (Val_class klass);
      List.iter
        (fun (mname, params, body) ->
           let fn = {
             name = mname;
             params;
             body;
             closure = methods_env;
             bound_this = None;
           } in
           Hashtbl.replace method_table mname fn)
        methods
  | Return (_tok, expr_opt) ->
      let v = match expr_opt with Some e -> evaluate env e | None -> Val_nil in
      raise (Return v)

let global_env = Environment.create ()

let interpret statements =
  try
    List.iter (execute global_env) statements;
    Ok ()
  with
  | RuntimeError (msg, line) -> Error (Printf.sprintf "Line %d: %s" line msg)
  | Return _ -> Error "Return statement outside of function."
