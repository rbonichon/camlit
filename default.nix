{ pkgs ? import <nixpkgs> { }
}:

rec {
  devshell = pkgs.mkShell {
    buildInputs = with pkgs;
      [
        dune_2
        ocaml
        ocamlPackages.merlin
        ocamlformat
      ];

  };
}
