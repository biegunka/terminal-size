{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc8107
, stack ? pkgs.stack
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    stack
  ];

  shellHook = ''
  '';
}
