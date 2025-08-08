(* lox/src/interpreter.ml *)

open Token
open Ast

(* Runtime values *)
type value =
  | Val_number of float
  | Val_string of string
  | Val_bool   of bool
  | Val_nil

exception RuntimeError of string * int

module Environment = struct
  type t = (string, value) Hashtbl.t
  let create () = Hashtbl.create 16
  let define env name v = Hashtbl.replace env name v
  let get env name =
    match Hashtbl.find_opt env name with
    | Some v -> v
    | None   -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1))
  let assign env name v =
    if Hashtbl.mem env name then Hashtbl.replace env name v
    else raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1))
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
  | Literal lit ->
      literal_to_value lit

  | Variable name ->
      Environment.get env name

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

let rec execute env = function
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
      List.iter (execute env) stmts

let global_env = Environment.create ()

let interpret statements =
  try
    List.iter (execute global_env) statements;
    Ok ()
  with RuntimeError (msg, line) ->
    Error (Printf.sprintf "Line %d: %s" line msg)
