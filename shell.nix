{ compiler ? "ghc883"
}:

let
  pkgs = import <nixpkgs> {};
  hspkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {};
  };
  drv = hspkgs.callPackage ./default.nix {};
  ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs);
in pkgs.mkShell {
  buildInputs = [ ghc hspkgs.cabal-install ];
}
