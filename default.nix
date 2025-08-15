{ pkgs ? import ./nixpkgs-pinned.nix }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [cabal-install
       ghcid
       hasktags
       stylish-haskell
       hpack
      ]);
}
