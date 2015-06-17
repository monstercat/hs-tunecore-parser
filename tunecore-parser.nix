{ mkDerivation,
  attoparsec,
  base,
  bytestring,
  cassava,
  stdenv,
  text,
  time,
}:
mkDerivation {
  pname = "tunecore-parser";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base
                   bytestring
                   attoparsec
                   cassava
                   text
                   time
                 ];
  homepage = "https://github.com/monstercat/hs-tunecore-parser";
  description = "Tunecore CSV Parser";
  license = stdenv.lib.licenses.mit;
}
