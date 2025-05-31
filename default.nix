{ mkDerivation,
  base, bytestring, lib, mtl, protolude, text, url, aeson, blaze-html, data-default,
  fsnotify, hashing, http-client, http-types, lens, magic, modern-uri,
  monad-logger, nonempty-containers, ordered-containers, scotty, split, temporary, wai-extra,
  wreq, yaml, optparse-applicative
}:
mkDerivation {
  name = "rainbow-hash";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring mtl protolude text aeson blaze-html data-default
    fsnotify hashing http-client http-types lens magic modern-uri
    monad-logger nonempty-containers ordered-containers scotty split
    temporary wai-extra wreq yaml optparse-applicative
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
