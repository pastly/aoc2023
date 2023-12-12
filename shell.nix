{ pkgs ? import <nixpkgs> {} }:

let 
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: [
    pkgs.regex-posix
    pkgs.memoize
    #pkgs.parallel
  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    #haskell-language-server
    #cabal-install

    tup
    entr
    python311
  ];
}
