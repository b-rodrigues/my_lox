open Token
open Ast

(* Runtime values *)
type value =
  | Val_number of float
  | Val_string of string
  | Val_bool of bool
  | Val_nil
[@@deriving show]

exception RuntimeError of string * int

(* Environment for variables *)
module Environment = struct
  type t = (string, value) Hashtbl.t

  let create () = Hashtbl.create 16

  let define env name value =
    Hashtbl.replace env name value

  let get env name =
    match Hashtbl.find_opt env name with
    | Some value -> value
    | None -> raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1)) (* Line number is a placeholder *)

  let assign env name value =
    if Hashtbl.mem env name then
      Hashtbl.replace env name value
    else
      raise (RuntimeError ("Undefined variable '" ^ name ^ "'.", 1)) (* Line number is a placeholder *)
end

(* Convert literals to values *)
let literal_to_value = function
  | Lit_number n -> Val_number n
  | Lit_string s -> Val_string s
  | Lit_bool b -> Val_bool b
  | Lit_nil -> Val_nil

(* Convert values to strings for printing *)
let value_to_string = function
  | Val_number n ->
      if n = Float.round n then
        Printf.sprintf "%.0f" n
      else
        string_of_float n
  | Val_string s -> s
  | Val_bool true -> "true"
  | Val_bool false -> "false"
  | Val_nil -> "nil"

(* Check if value is truthy (everything except false and nil is truthy) *)
let is_truthy = function
  | Val_bool false | Val_nil -> false
  | _ -> true

(* Check if two values are equal *)
let is_equal v1 v2 =
  match (v1, v2) with
  | (Val_nil, Val_nil) -> true
  | (Val_nil, _) | (_, Val_nil) -> false
  | (Val_bool b1, Val_bool b2) -> b1 = b2
  | (Val_number n1, Val_number n2) -> n1 = n2
  | (Val_string s1, Val_string s2) -> s1 = s2
  | _ -> false

(* Evaluate expressions *)
let rec evaluate env = function
  | Literal lit -> literal_to_value lit
  
  | Variable name -> Environment.get env name
  
  | Grouping expr -> evaluate env expr
  
  | Unary (operator, right) ->
      let right_val = evaluate env right in
      (match operator.token_type with
       | MINUS ->
           (match right_val with
            | Val_number n -> Val_number (-.n)
            | _ -> raise (RuntimeError ("Operand must be a number.", operator.line)))
       | BANG -> Val_bool (not (is_truthy right_val))
       | _ -> raise (RuntimeError ("Unknown unary operator.", operator.line)))

  (* --- MODIFIED: Handle binary expressions --- *)
  | Binary (left, operator, right) ->
      (match operator.token_type with
       (* Special cases for logical operators with short-circuiting *)
       | OR ->
           let left_val = evaluate env left in
           if is_truthy left_val then left_val
           else evaluate env right
       | AND ->
           let left_val = evaluate env left in
           if not (is_truthy left_val) then left_val
           else evaluate env right

       (* Default case for all other binary operators (eager evaluation) *)
       | _ ->
           let left_val = evaluate env left in
           let right_val = evaluate env right in
           (match operator.token_type with
            | MINUS ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) -> Val_number (l -. r)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
            | PLUS ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) -> Val_number (l +. r)
                 | (Val_string l, Val_string r) -> Val_string (l ^ r)
                 | _ -> raise (RuntimeError ("Operands must be two numbers or two strings.", operator.line)))
            | SLASH ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) ->
                     if r = 0.0 then
                       raise (RuntimeError ("Division by zero.", operator.line))
                     else
                       Val_number (l /. r)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
            | STAR ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) -> Val_number (l *. r)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
            | GREATER ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) -> Val_bool (l > r)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
            | GREATER_EQUAL ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) -> Val_bool (l >= r)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
            | LESS ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) -> Val_bool (l < r)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
            | LESS_EQUAL ->
                (match (left_val, right_val) with
                 | (Val_number l, Val_number r) -> Val_bool (l <= r)
                 | _ -> raise (RuntimeError ("Operands must be numbers.", operator.line)))
            | BANG_EQUAL -> Val_bool (not (is_equal left_val right_val))
            | EQUAL_EQUAL -> Val_bool (is_equal left_val right_val)
            | _ -> raise (RuntimeError ("Unknown binary operator.", operator.line))))

(* Execute statements *)
let execute env = function
  | Expression expr ->
      let _ = evaluate env expr in
      ()

  | Print expr ->
      let value = evaluate env expr in
      Printf.printf "%s\n" (value_to_string value);
      flush stdout

  | Var (name, init_expr) ->
      let value = match init_expr with
        | Some expr -> evaluate env expr
        | None -> Val_nil
      in
      Environment.define env name value

(* Global environment for REPL persistence *)
let global_env = Environment.create ()

(* Interpret a list of statements *)
let interpret statements =
  try
    List.iter (execute global_env) statements;
    Ok ()
  with
  | RuntimeError (msg, line) -> Error (Printf.sprintf "Line %d: %s" line msg)
