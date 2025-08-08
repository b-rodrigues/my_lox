# Lox (OCaml) — an interpreter with resolver, closures, and keyword args

This repository contains a Lox interpreter written in OCaml. It broadly follows
Crafting Interpreters (expressions, statements, variables, control flow,
functions, closures) with some additions:

- A resolver pass that performs lexical resolution before execution for faster
  lookups and better diagnostics.

- Keyword (named) arguments for both user-defined and native functions.


- A small set of built-ins, currently including a `clock` function.

## Features

- Lexical scoping with closures
- Variables, blocks, if/else, while, for (as syntactic sugar), function
  declarations and calls, return
- Early errors from the resolver:
  - “can’t read a local variable in its own initializer”
  - “return outside function”
- Named arguments inside calls: `f(x = 1, y = 2)`; rule: positional args cannot follow named args
- Built-ins registered at startup
  - `clock(time = "unix" | "human")`

## Quick start (with Nix shell)

Prerequisites:
- Nix installed

Enter the Nix shell (provides OCaml, dune, and dependencies):
```bash
nix-shell
```

Build:
```bash
dune build
```

Run the REPL:
```bash
dune exec lox
```

Run a script:
```bash
dune exec lox -- path/to/script.lox
```

Notes:
- In the REPL: if you enter a single expression (not a statement), the REPL prints its value automatically.
- Errors show line numbers; many name/scope mistakes are caught during resolution before the program runs.

## Project layout

- src/
  - token.ml, scanner.ml — tokenize input
  - ast.ml — AST types (expressions and statements)
  - parser.ml — parses tokens into AST (supports keyword args)
  - resolver.ml — pre-execution scope resolution (computes lexical distances, validates some rules)
  - interpreter.ml — evaluates the resolved AST
  - builtins.ml — registration of native functions (e.g., `clock`)
  - main.ml — CLI entry point and REPL
  - dune — dune project definition

## Built-ins

- `clock(time = "unix" | "human")`
  - `time = "unix"` returns a number (seconds since epoch)
  - `time = "human"` returns a formatted string `YYYY-MM-DD HH:MM:SS`
  - Default is `"unix"` if omitted.

Examples:
```lox
print clock();                    // e.g., 1723123456
print clock(time = "unix");       // same as above
print clock(time = "human");      // e.g., 2025-08-08 15:43:05
```

## Language basics

Statements:
- Expression statement: `expr;`
- Print statement: `print expr;`
- Variable declaration: `var name = initializer?;`
- Block: `{ statement* }`
- If: `if (cond) statement else statement?`
- While: `while (cond) statement`
- For: `for (init?; cond?; increment?) statement` (parsed into a `while` loop)
- Function declaration: `fun name(params) { body }`
- Return: `return expr?;`

Expressions:
- Literals: numbers, strings, `true`, `false`, `nil`
- Grouping: `(expr)`
- Unary: `-expr`, `!expr`
- Binary: `+`, `-`, `*`, `/`, `<`, `<=`, `>`, `>=`, `==`, `!=`, `and`, `or`
- Variable and assignment: `name`, `name = expr`
- Call: `callee(arg1, arg2, ...)`
  - Named args: `callee(x = 1, y = 2)`; positional args cannot follow named args.

### Syntax examples

Hello world and variables:
```lox
print "Hello, world!";
var a = 41;
a = a + 1;
print a; // 42
```

Blocks and shadowing:
```lox
var a = "outer";
{
  var a = "inner";
  print a; // inner
}
print a;   // outer
```

If/else and while:
```lox
var n = 3;
if (n > 0) {
  print "positive";
} else {
  print "non-positive";
}

while (n > 0) {
  print n;
  n = n - 1;
}
```

For loop (sugar; becomes a while internally):
```lox
for (var i = 0; i < 3; i = i + 1) {
  print i; // 0, 1, 2
}
```

Functions, closures, and named args:
```lox
fun makeAdder(n) {
  fun add(x) {
    return x + n;
  }
  return add;
}

var add2 = makeAdder(2);
print add2(40); // 42

fun greet(name, punct) { print name + punct; }
greet(name = "Alice", punct = "!");
greet("Bob", punct = ".");
```

Resolver-detected errors (caught before running):
```lox
{
  var a = a;       // Error: Can't read local variable in its own initializer.
}

fun f() {
  return 42;
}
return 1;          // Error: Can't return from top-level code.
```

## Hacking on it

Inside the Nix shell:
- Build: `dune build`
- Run REPL: `dune exec lox`
- Run a file: `dune exec lox -- examples/hello.lox`

Modifying the interpreter:
- Add language features by extending:
  - `ast.ml` (add node types)
  - `parser.ml` (recognize new syntax)
  - `resolver.ml` (track scope rules / distances)
  - `interpreter.ml` (evaluate new nodes)
- Add or change native functions in `src/builtins.ml`. Register new natives in `Builtins.register_all`.

Troubleshooting:
- Resolver errors happen before execution and reference a line number in your source.
- If you see “Undefined variable 'x'.” at runtime, it’s typically a genuine runtime lookup (e.g., referencing an undeclared global or after its scope ended).

## Roadmap ideas

- Arrays and maps (with indexing and literals)
- Default parameter values and better diagnostics for missing named parameters
- `break`/`continue` for loops
- Modules/imports
- More built-ins (math, io, time, data utilities)
- Classes/this/super (Crafting Interpreters OOP chapter)

## License

EUPL v1.2
