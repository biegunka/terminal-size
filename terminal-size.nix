{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "terminal-size";
  version = "0.3.2";
  src = ./.;
  buildDepends = [ base ];
  description = "Get terminal window height and width";
  license = stdenv.lib.licenses.bsd3;
}
