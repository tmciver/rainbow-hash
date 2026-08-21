{ pkgs ? import ./nixpkgs-pinned.nix }:

let
  rainbow-hash = pkgs.haskell.lib.justStaticExecutables (
    pkgs.haskellPackages.callCabal2nix "rainbow-hash" ./. {}
  );
in
pkgs.dockerTools.buildLayeredImage {
  name = "com.timmciver/rainbow-hash";
  tag = "latest";
  contents = [ rainbow-hash ];
  extraCommands = ''
    mkdir -p tmp
    chmod 1777 tmp
  '';
  config = {
    Entrypoint = [ "/bin/rh-server" ];
    ExposedPorts = {
      "3000/tcp" = {};
    };
  };
}
