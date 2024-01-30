{ pkgs ? import ./nixpkgs {} }:
let
  overrides = self: super: {
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.2.0.2";
      sha256 = "08krfgnjwn9791lwq6azvnj8wy0b1ivyndyhipnrip202vv30rl0";
    } {};
  };
  targets = ["ghc8107" "ghc981"];

  ghcs = pkgs.lib.genAttrs targets (target: pkgs.haskell.packages.${target}.override {
    inherit overrides;
  });
in pkgs.lib.mapAttrs
    (_: ghc: ghc.callCabal2nix "coquina" (pkgs.lib.cleanSource ./.) {}) ghcs
