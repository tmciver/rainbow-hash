{ mkDerivation,
  base, bytestring, lib, mtl, protolude, text, url, aeson, blaze-html, data-default,
  fsnotify, hashing, http-client, http-types, lens, magic, modern-uri,
  monad-logger, nonempty-containers, scotty, split, temporary, wai-extra,
  wreq, yaml
}:
mkDerivation {
  pname = "serenity-fetch";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring mtl protolude text aeson blaze-html data-default
    fsnotify hashing http-client http-types lens magic modern-uri
    monad-logger nonempty-containers scotty split temporary wai-extra
    wreq yaml
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
