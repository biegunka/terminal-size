{ cabal }:

cabal.mkDerivation (self: {
  pname = "terminal-size";
  version = "0.3.0";
  src = ./.;
  meta = {
    description = "Get terminal window height and width";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
