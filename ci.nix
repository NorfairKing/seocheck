let
  pkgs = import ./nix/pkgs.nix;
  pre-commit = import ./nix/pre-commit.nix;
in
pkgs.seoCheckPackages // {
  pre-commit-check = pre-commit.check;
}
