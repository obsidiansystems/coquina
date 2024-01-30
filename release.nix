{ pkgs ? import ./nixpkgs {} }:
let
  overrides = self: super: {
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.2.0.2";
      sha256 = "08krfgnjwn9791lwq6azvnj8wy0b1ivyndyhipnrip202vv30rl0";
    } {};
    tasty-hedgehog = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callHackageDirect {
      pkg = "tasty-hedgehog";
      ver = "1.4.0.2";
      sha256 = "04kg2qdnsqzzmj3xggy2jcgidlp21lsjkz4sfnbq7b1yhrv2vbbc";
    } {}));
  };
  targets = ["ghc8107" "ghc981"];

  ghcs = pkgs.lib.genAttrs targets (target: pkgs.haskell.packages.${target}.override {
    inherit overrides;
  });
in pkgs.lib.mapAttrs
    (_: ghc: ghc.callCabal2nix "coquina" (pkgs.lib.cleanSource ./.) {}) ghcs
