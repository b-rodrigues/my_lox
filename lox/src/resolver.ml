open Ast

exception ResolveError of string * int

type function_type =
  | FT_none
  | FT_function
  | FT_initializer

type class_type =
  | CT_none
  | CT_class

type scope = (string, bool) Hashtbl.t  (* name -> defined? *)

type t = {
  mutable scopes : scope list;         (* innermost first (head) *)
  mutable current_function : function_type;
  mutable current_class : class_type;
}

let create () = { scopes = []; current_function = FT_none; current_class = CT_none }

let push_scope r =
  r.scopes <- (Hashtbl.create 16) :: r.scopes

let pop_scope r =
  match r.scopes with
  | _ :: rest -> r.scopes <- rest
  | [] -> ()

let declare r name line =
  match r.scopes with
  | [] -> ()  (* global; no tracking needed *)
  | scope :: _ ->
      if Hashtbl.mem scope name then
        raise (ResolveError (Printf.sprintf "Variable '%s' already declared in this scope." name, line));
      Hashtbl.replace scope name false

let define r name =
  match r.scopes with
  | [] -> ()
  | scope :: _ -> Hashtbl.replace scope name true

let rec resolve_stmts r (stmts : stmt list) : stmt list =
  List.map (resolve_stmt r) stmts

and resolve_stmt r = function
  | Expression e -> Expression (resolve_expr r e)
  | Print e -> Print (resolve_expr r e)
  | Var (name, init_opt) ->
      declare r name 0;
      let init_opt' = Option.map (resolve_expr r) init_opt in
      define r name;
      Var (name, init_opt')
  | Block stmts ->
      push_scope r;
      let stmts' = resolve_stmts r stmts in
      pop_scope r;
      Block stmts'
  | If (cond, then_b, else_b) ->
      let cond' = resolve_expr r cond in
      let then_b' = resolve_stmt r then_b in
      let else_b' = Option.map (resolve_stmt r) else_b in
      If (cond', then_b', else_b')
  | While (cond, body) ->
      let cond' = resolve_expr r cond in
      let body' = resolve_stmt r body in
      While (cond', body')
  | Fun (name, params, body) ->
      declare r name 0;
      define r name;
      let enclosing_fn = r.current_function in
      r.current_function <- FT_function;
      push_scope r;
      List.iter (fun p -> declare r p 0; define r p) params;
      let body' = resolve_stmts r body in
      pop_scope r;
      r.current_function <- enclosing_fn;
      Fun (name, params, body')
  | Class (name, methods) ->
      declare r name 0;
      define r name;
      let enclosing_class = r.current_class in
      r.current_class <- CT_class;
      (* Methods *)
      List.iter
        (fun (mname, params, _body) ->
          let enclosing_fn = r.current_function in
          let fn_type = if mname = "init" then FT_initializer else FT_function in
          r.current_function <- fn_type;
          push_scope r;
          (* 'this' in method scope *)
          declare r "this" 0; define r "this";
          List.iter (fun p -> declare r p 0; define r p) params;
          ignore (resolve_stmts r _body);
          pop_scope r;
          r.current_function <- enclosing_fn
        ) methods;
      r.current_class <- enclosing_class;
      Class (name, methods)
  | Return (tok, expr_opt) ->
      if r.current_function = FT_none then
        raise (ResolveError ("Can't return from top-level code.", tok.Token.line));
      (match r.current_function, expr_opt with
       | FT_initializer, Some _ ->
           raise (ResolveError ("Can't return a value from an initializer.", tok.Token.line))
       | _ -> ());
      let expr_opt' = Option.map (resolve_expr r) expr_opt in
      Return (tok, expr_opt')

and resolve_expr r = function
  | Literal _ as e -> e
  | Grouping e -> Grouping (resolve_expr r e)
  | Unary (tok, e) -> Unary (tok, resolve_expr r e)
  | Binary (l, tok, rgt) -> Binary (resolve_expr r l, tok, resolve_expr r rgt)
  | Call (callee, tok, args) ->
      let callee' = resolve_expr r callee in
      let args' =
        List.map
          (function
            | Arg_pos e -> Arg_pos (resolve_expr r e)
            | Arg_named (name, e, line) -> Arg_named (name, resolve_expr r e, line))
          args
      in
      Call (callee', tok, args')
  | Get (obj, name, line) ->
      let obj' = resolve_expr r obj in
      Get (obj', name, line)
  | Set (obj, name, value, line) ->
      let obj' = resolve_expr r obj in
      let value' = resolve_expr r value in
      Set (obj', name, value', line)
  | Variable (name, _, line) as v ->
      if name = "this" && r.current_class = CT_none then
        raise (ResolveError ("Can't use 'this' outside of a class.", line));
      (* Can't read local variable in its own initializer *)
      (match r.scopes with
       | scope :: _ ->
           (match Hashtbl.find_opt scope name with
            | Some false ->
                raise (ResolveError ("Can't read local variable in its own initializer.", line))
            | _ -> ())
       | [] -> ());
      let depth_opt = resolve_local r name in
      (match v with
       | Variable (_, _, _) -> Variable (name, depth_opt, line)
       | _ -> v)
  | Assign (name, value, _, line) ->
      let value' = resolve_expr r value in
      let depth_opt = resolve_local r name in
      Assign (name, value', depth_opt, line)

and resolve_local r name : int option =
  let rec find scopes idx =
    match scopes with
    | [] -> None
    | scope :: rest ->
        if Hashtbl.mem scope name
        then Some idx
        else find rest (idx + 1)
  in
  find r.scopes 0

let resolve (stmts : stmt list) : (stmt list, string) result =
  try
    let r = create () in
    (* synthetic global scope *)
    push_scope r;
    let resolved = resolve_stmts r stmts in
    pop_scope r;
    Ok resolved
  with
  | ResolveError (msg, line) ->
      Error (if line > 0 then Printf.sprintf "Line %d: %s" line msg else msg)
