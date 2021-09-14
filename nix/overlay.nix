final: previous:
with final.haskell.lib;

{
  seoCheckPackages =
    let
      seoCheckPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) { }
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
      seoCheckPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (seoCheckPkg name);
      seoCheckPkgWithOwnComp = name: seoCheckPkgWithComp name name;
    in
    {
      "seocheck" = seoCheckPkgWithOwnComp "seocheck";
    };
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
                final.seoCheckPackages
            );
      }
    );
}
