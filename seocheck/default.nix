{ mkDerivation, aeson, base, bytestring, case-insensitive, conduit
, containers, html-conduit, http-client, http-client-tls
, http-types, lib, monad-logger, network-uri, opt-env-conf
, opt-env-conf-test, path, path-io, pretty-show, safe-coloured-text
, safe-coloured-text-terminfo, stm, sydtest, sydtest-discover, text
, unliftio, validity, xml-conduit
}:
mkDerivation {
  pname = "seocheck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive conduit containers
    html-conduit http-client http-client-tls http-types monad-logger
    network-uri opt-env-conf path path-io pretty-show
    safe-coloured-text safe-coloured-text-terminfo stm text unliftio
    validity xml-conduit
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/seocheck#readme";
  description = "Check for common SEO mistakes on CI";
  license = lib.licenses.mit;
  mainProgram = "seocheck";
}
