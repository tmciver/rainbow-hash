{ ... }:

let
  pkgs = import ./nixpkgs-pinned.nix;
  project = (import ./release.nix {}).project1;
  devDeps = [ pkgs.haskellPackages.haskell-language-server
#              pkgs.haskellPackages.ghcid
#              pkgs.haskellPackages.cabal-install
              pkgs.emacs
              pkgs.git
            ];
in
pkgs.lib.overrideDerivation project.env (old:
  { buildInputs = old.buildInputs ++ devDeps; }
)
