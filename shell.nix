#{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/75f4ba05c63be3f147bcc2f7bd4ba1f029cedcb1.tar.gz") { } }:
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz") { } }:

pkgs.haskellPackages.developPackage {
  root = ./.;
}
