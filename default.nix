# OCaml development environment for "Crafting Interpreters in OCaml"
let
  pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2025-08-04.tar.gz") {};

  ocaml_env = pkgs.ocamlPackages;

  system_packages = builtins.attrValues {
    inherit (pkgs)
      glibcLocales
      opam
      nix;
  };

  ocaml_packages = with ocaml_env; [
    dune_3
    findlib
    merlin
    ocaml
    ocaml-lsp
    ocamlformat
    ppx_deriving
    ppxlib
    utop
  ];

  shell = pkgs.mkShell {
    LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";

    buildInputs = ocaml_packages ++ system_packages;

  };
in
{
  inherit pkgs shell;
}
