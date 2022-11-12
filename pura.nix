{ mkDerivation, base, bytestring, containers, directory, filepath
, lib, optparse-applicative, QuickCheck, tasty, tasty-quickcheck
, text, unix, yaml
}:
mkDerivation {
  pname = "pura";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring containers text yaml ];
  executableHaskellDepends = [
    base directory filepath optparse-applicative unix
  ];
  testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
  license = "unknown";
  mainProgram = "pura";
}
