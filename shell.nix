with import <nixpkgs> { };
mkShell {
  packages = [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.ormolu
    haskellPackages.cabal2nix
  ];
  shellHook = ''
    alias run="cabal run pura --"
    alias b="cabal build"
    alias t="cabal test"
    alias fmt="ormolu -i app/**/*.hs lib/**/*.hs test/**/*.hs"
    function x() {
      mkdir bin
      cabal install --installdir ./bin exe:pura
    }
    alias release="cabal2nix . > pura.nix && nix-build"
  '';
}

