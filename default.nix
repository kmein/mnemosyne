{ mkDerivation, base, bytestring, cassava, hasmin, HaTeX, lucid
, megaparsec, optparse-applicative, prettyprinter, raw-strings-qq
, stdenv, text
}:
mkDerivation {
  pname = "mnemosyne";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava hasmin HaTeX lucid megaparsec prettyprinter
    raw-strings-qq text
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/kmein/mnemosyne#readme";
  description = "A tool for managing a database of literature quotes";
  license = stdenv.lib.licenses.mit;
}
