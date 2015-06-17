{ mkDerivation, base, cassava, stdenv }:
mkDerivation {
  pname = "tunecore-parser";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base cassava ];
  homepage = "https://github.com/monstercat/hs-tunecore-parser";
  description = "Tunecore CSV Parser";
  license = stdenv.lib.licenses.mit;
}
