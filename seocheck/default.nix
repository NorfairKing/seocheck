{ mkDerivation, base, bytestring, case-insensitive, containers
, html-conduit, http-client, http-client-tls, http-types, lib
, list-t, monad-logger, network-uri, opt-env-conf
, opt-env-conf-test, pretty-show, retry, safe-coloured-text
, safe-coloured-text-terminfo, stm-containers, sydtest
, sydtest-discover, text, unliftio, validity, validity-network-uri
, xml-conduit
}:
mkDerivation {
  pname = "seocheck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers html-conduit
    http-client http-client-tls http-types list-t monad-logger
    network-uri opt-env-conf pretty-show retry safe-coloured-text
    safe-coloured-text-terminfo stm-containers text unliftio validity
    validity-network-uri xml-conduit
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/seocheck#readme";
  description = "Check for common SEO mistakes on CI";
  license = lib.licenses.mit;
  mainProgram = "seocheck";
}
