open Ast

exception ResolveError of string * int

(* Function kinds *)
type function_type =
  | FT_none
  | FT_function
  | FT_initializer

(* Class kinds *)
type class_type =
  | CT_none
  | CT_class
  | CT_subclass

(* A scope maps variable name -> is_defined yet?  (false means declared, not yet fully defined) *)
type scope = (string, bool) Hashtbl.t

type t = {
  mutable scopes : scope list;            (* innermost scope is head of list *)
  mutable current_function : function_type;
  mutable current_class : class_type;
}

let create () = {
  scopes = [];
  current_function = FT_none;
  current_class = CT_none;
}

let push_scope r =
  r.scopes <- (Hashtbl.create 16) :: r.scopes

let pop_scope r =
  match r.scopes with
  | _ :: rest -> r.scopes <- rest
  | [] -> ()

let declare r name line =
  match r.scopes with
  | [] -> ()
  | scope :: _ ->
      if Hashtbl.mem scope name then
        raise (ResolveError (Printf.sprintf "Variable '%s' already declared in this scope." name, line));
      Hashtbl.replace scope name false

let define r name =
  match r.scopes with
  | [] -> ()
  | scope :: _ -> Hashtbl.replace scope name true

let rec resolve_stmts r stmts =
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
      let body' = resolve_function_body r ~kind:FT_function ~params ~body in
      Fun (name, params, body')
  | Class (name, superclass_opt, methods) ->
      (* Declare & define class name so methods can reference it *)
      declare r name 0;
      define r name;
      let enclosing_class = r.current_class in
      (* Resolve superclass if any *)
      let (superclass_opt', class_kind) =
        match superclass_opt with
        | None -> (None, CT_class)
        | Some (Variable (sname, _, line) as super_expr) ->
            if sname = name then
              raise (ResolveError ("A class can't inherit from itself.", line));
            let resolved_super = resolve_expr r super_expr in
            (Some resolved_super, CT_subclass)
        | Some _ -> (superclass_opt, CT_subclass)  (* Shouldn't happen; parser builds Variable. *)
      in
      r.current_class <- class_kind;

      (* IMPORTANT: Scope layering MUST mirror runtime:
         Runtime chain when calling a method of subclass:
           params_scope (depth 0)
           this_scope   (depth 1)
           super_scope  (depth 2)    (only for subclass)
           ... outer env ...
         So we push scopes in reverse order (outer first) so that the list head
         (innermost) ends up being params, then this, then super. *)

      (* For subclass: push a scope for 'super' (outermost of the three) *)
      (match superclass_opt' with
       | Some _ ->
           push_scope r;
           declare r "super" 0; define r "super"
       | None -> ());

      let methods' =
        List.map
          (fun (mname, params, body) ->
             let fn_type =
               if mname = "init" then FT_initializer else FT_function
             in
             let enclosing_fn = r.current_function in
             r.current_function <- fn_type;

             (* Push 'this' scope (will become middle layer) *)
             push_scope r;
             declare r "this" 0; define r "this";

             (* Push params scope (innermost) *)
             push_scope r;
             List.iter (fun p -> declare r p 0; define r p) params;

             let body' = resolve_stmts r body in

             (* Pop in reverse order *)
             pop_scope r;      (* params *)
             pop_scope r;      (* this *)

             r.current_function <- enclosing_fn;
             (mname, params, body'))
          methods
      in

      (* Pop 'super' scope if we created it *)
      (match superclass_opt' with
       | Some _ -> pop_scope r
       | None -> ());

      r.current_class <- enclosing_class;
      Class (name, superclass_opt', methods')
  | Return (tok, expr_opt) ->
      if r.current_function = FT_none then
        raise (ResolveError ("Can't return from top-level code.", tok.Token.line));
      (match r.current_function, expr_opt with
       | FT_initializer, Some _ ->
           raise (ResolveError ("Can't return a value from an initializer.", tok.Token.line))
       | _ -> ());
      let expr_opt' = Option.map (resolve_expr r) expr_opt in
      Return (tok, expr_opt')

and resolve_function_body r ~kind ~params ~body =
  let enclosing_fn = r.current_function in
  r.current_function <- kind;
  (* For normal functions/lambdas we have only one scope for params (no 'this', 'super') *)
  push_scope r;
  List.iter (fun p -> declare r p 0; define r p) params;
  let body' = resolve_stmts r body in
  pop_scope r;
  r.current_function <- enclosing_fn;
  body'

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
            | Arg_named (n, e, line) -> Arg_named (n, resolve_expr r e, line))
          args
      in
      Call (callee', tok, args')
  | Get (obj, name, line) ->
      Get (resolve_expr r obj, name, line)
  | Set (obj, name, value, line) ->
      Set (resolve_expr r obj, name, resolve_expr r value, line)
  | Super (tok, method_name, line, _) ->
      if r.current_class = CT_none then
        raise (ResolveError ("Can't use 'super' outside of a class.", line));
      if r.current_class <> CT_subclass then
        raise (ResolveError ("Can't use 'super' in a class with no superclass.", line));
      let depth_opt = resolve_local r "super" in
      Super (tok, method_name, line, depth_opt)
  | Variable (name, _, line) ->
      if name = "this" && r.current_class = CT_none then
        raise (ResolveError ("Can't use 'this' outside of a class.", line));
      if name = "super" && r.current_class <> CT_subclass then
        raise (ResolveError ("Can't use 'super' in a class with no superclass.", line));
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
  | Lambda (params, body, line) ->
      let body' = resolve_function_body r ~kind:FT_function ~params ~body in
      Lambda (params, body', line)

and resolve_local r name : int option =
  let rec find scopes idx =
    match scopes with
    | [] -> None
    | scope :: rest ->
        if Hashtbl.mem scope name then Some idx
        else find rest (idx + 1)
  in
  find r.scopes 0

let resolve stmts =
  try
    let r = create () in
    (* Global scope (depth grows outward) *)
    push_scope r;
    let resolved = resolve_stmts r stmts in
    pop_scope r;
    Ok resolved
  with
  | ResolveError (msg, line) ->
      Error (if line > 0 then Printf.sprintf "Line %d: %s" line msg else msg)
