let
  pkgs = import <nixpkgs> { };
in (import ./default.nix).shellFor {
  withHoogle = true;

  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    ormolu = "0.3.1.0";
  };

  # All dependencies should be provided by nix
  exactDeps = true;
}
