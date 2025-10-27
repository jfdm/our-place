{ mkDerivation, aeson, array, base, bytestring, containers
, crypton-connection, crypton-x509-system, data-default, directory
, feed, filepath, http-client, http-client-tls, http-types, lib
, pandoc, regex-pcre-builtin, text, time, tls, xml-types, yaml
, http-semantics, fsnotify, wai-logger, http2, warp, wai-extra, wai-app-static, hakyll
}:
mkDerivation {
  pname = "our-place";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array base bytestring containers crypton-connection
    crypton-x509-system data-default directory feed filepath
    http-client http-client-tls http-types pandoc regex-pcre-builtin
    text time tls xml-types yaml
    http-semantics fsnotify wai-logger fsnotify http2 warp wai-extra wai-app-static hakyll
  ];
  homepage = "https://github.com/jfdm/our-place";
  description = "An artisanal course content system";
  license = "unknown";
}
