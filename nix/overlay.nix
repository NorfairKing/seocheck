final: previous:
with final.haskell.lib;

let
  seocheck =
    generateOptparseApplicativeCompletion "seocheck" (
      buildStrictly (
        disableLibraryProfiling (
          final.haskellPackages.callCabal2nixWithOptions "seocheck" (final.gitignoreSource (../seocheck)) "--no-hpack" { }
        )
      ));
in
{
  seocheck = justStaticExecutables seocheck;
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                { inherit seocheck; }
            );
      }
    );
}
