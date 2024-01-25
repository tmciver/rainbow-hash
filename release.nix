with (import ./nixpkgs-pin.nix);
{
  pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) {}
}:
{ project1 = pkgs.haskellPackages.callPackage ./default.nix { };
}
