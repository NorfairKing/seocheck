{ mkDerivation, aeson, base, bytestring, case-insensitive, conduit
, containers, html-conduit, http-client, http-client-tls
, http-types, lib, list-t, monad-logger, network-uri, opt-env-conf
, opt-env-conf-test, path, path-io, pretty-show, retry
, safe-coloured-text, safe-coloured-text-terminfo, stm
, stm-containers, sydtest, sydtest-discover, text, unliftio
, validity, validity-network-uri, xml-conduit
}:
mkDerivation {
  pname = "seocheck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive conduit containers
    html-conduit http-client http-client-tls http-types list-t
    monad-logger network-uri opt-env-conf path path-io pretty-show
    retry safe-coloured-text safe-coloured-text-terminfo stm
    stm-containers text unliftio validity validity-network-uri
    xml-conduit
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/seocheck#readme";
  description = "Check for common SEO mistakes on CI";
  license = lib.licenses.mit;
  mainProgram = "seocheck";
}
