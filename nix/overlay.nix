final: prev:
with final.haskell.lib;

{
  seocheck = justStaticExecutables final.haskellPackages.seocheck;
  haskellPackages =
    prev.haskellPackages.override (old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super: {
          seocheck = self.opt-env-conf.installManpagesAndCompletions [ "seocheck" ] (buildStrictly (self.callPackage ../seocheck { }));
        }
      );
    }
    );
}
