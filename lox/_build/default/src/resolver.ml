open Ast

exception ResolveError of string * int

type function_type =
  | FT_none
  | FT_function

type scope = (string, bool) Hashtbl.t  (* name -> defined? *)

type t = {
  mutable scopes : scope list;         (* innermost first (head) *)
  mutable current_function : function_type;
}

let create () = { scopes = []; current_function = FT_none }

let push_scope r =
  r.scopes <- (Hashtbl.create 16) :: r.scopes

let pop_scope r =
  match r.scopes with
  | _ :: rest -> r.scopes <- rest
  | [] -> ()

let declare r name line =
  match r.scopes with
  | [] -> ()  (* global; nothing to track *)
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
      (* Function name is bound in the enclosing scope *)
      declare r name 0;
      define r name;
      (* Resolve function body in a new scope with params *)
      let enclosing = r.current_function in
      r.current_function <- FT_function;
      push_scope r;
      List.iter (fun p -> declare r p 0; define r p) params;
      let body' = resolve_stmts r body in
      pop_scope r;
      r.current_function <- enclosing;
      Fun (name, params, body')
  | Return (tok, expr_opt) ->
      if r.current_function = FT_none then
        raise (ResolveError ("Can't return from top-level code.", tok.Token.line));
      let expr_opt' = Option.map (resolve_expr r) expr_opt in
      Return (tok, expr_opt')

and resolve_expr r = function
  | Literal _ as e -> e
  | Grouping e -> Grouping (resolve_expr r e)
  | Unary (tok, e) -> Unary (tok, resolve_expr r e)
  | Binary (l, tok, rgt) -> Binary (resolve_expr r l, tok, resolve_expr r rgt)
  | Call (callee, tok, args) ->
      let callee' = resolve_expr r callee in
      let args' = List.map (resolve_expr r) args in
      Call (callee', tok, args')
  | Variable (name, _, line) ->
      (* Error: reading a local variable in its own initializer *)
      (match r.scopes with
       | scope :: _ ->
           (match Hashtbl.find_opt scope name with
            | Some false ->
                raise (ResolveError ("Can't read local variable in its own initializer.", line))
            | _ -> ())
       | [] -> ());
      let depth_opt = resolve_local r name in
      Variable (name, depth_opt, line)
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
    (* Optional: track a synthetic global scope so top-level initializers are checked too *)
    push_scope r;
    let resolved = resolve_stmts r stmts in
    pop_scope r;
    Ok resolved
  with
  | ResolveError (msg, line) ->
      Error (if line > 0 then Printf.sprintf "Line %d: %s" line msg else msg)
