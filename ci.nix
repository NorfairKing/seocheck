let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
{
  inherit (pkgs) seocheck;
  pre-commit-check = pre-commit.run;
}
