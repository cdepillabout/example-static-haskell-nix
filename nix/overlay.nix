final: prev:

{
  # Static builds only work on ghc94, not ghc96+ :-(
  example-static-ghc-version = "ghc948";

  my-haskell =
    let
      myHaskellOverride = oldAttrs: {
        overrides =
          final.lib.composeExtensions
            (oldAttrs.overrides or (_: _: {}))
            (hfinal: hprev: {
              example-static-haskell-nix =
                let
                  filesToIgnore = [
                    ".git"
                    ".github"
                    ".stack-work"
                    "cabal.project"
                    "default.nix"
                    "flake.lock"
                    "flake.nix"
                    "nix"
                    "result"
                    "shell.nix"
                    "stack.yaml"
                    "stack.yaml.lock"
                  ];

                  src =
                    builtins.path {
                      name = "example-static-haskell-nix-src";
                      path = ./..;
                      filter = path: type:
                        with final.lib;
                        ! elem (baseNameOf path) filesToIgnore &&
                        ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
                    };

                  extraCabal2nixOptions = "";
                in
                hfinal.callCabal2nixWithOptions "example-static-haskell-nix" src extraCabal2nixOptions {};

            });
      };
    in
    prev.haskell // {
      packages = prev.haskell.packages // {
        native-bignum = prev.haskell.packages.native-bignum // {
          ${final.example-static-ghc-version} =
            prev.haskell.packages.native-bignum.${final.example-static-ghc-version}.override
              myHaskellOverride;
        };
      };
    };

  example-static-haskell-pkg-set = final.pkgsStatic.my-haskell.packages.native-bignum.${final.example-static-ghc-version};

  example-static = final.example-static-haskell-pkg-set.example-static-haskell-nix;

  example-static-just-exe =
    final.lib.pipe
      final.example-static
      [ final.haskell.lib.justStaticExecutables
      ];
}

