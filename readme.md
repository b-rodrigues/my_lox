This is the repo that contains my attempt at building Lox, a toy programming language
from the book [Crafting Interpreters](https://craftinginterpreters.com/) using Ocaml.

Mostly done through prompting LLMs.

Clone and build the dev env:

```bash
nix-shell
```

From the Nix shell, cd to the `lox/` directory and build using `dune`:

```bash
cd lox
dune build
```

Run the interpreter:

```bash
dune exec lox
```
