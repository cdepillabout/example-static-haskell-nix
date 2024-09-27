# This file defines a Nixpkgs overlay.
#
# This overlay adds two important attributes, `example-static-haskell-pkg-set`
# and `example-static-just-exe`.

final: prev:

{
  # The Nixpkgs GHC version to use.
  #
  # Note that as of this writing (2024-09-27), full static linking with
  # TemplateHaskell ONLY works on GHC-9.4 and earlier.  It DOES NOT work on
  # GHC-9.6, 9.8, or 9.10.
  #
  # See https://github.com/NixOS/nixpkgs/issues/275304 for more info.
  example-static-ghc-version = "ghc948";

  # This is effectively the same as Nixpkgs's top-level `haskell` attribute,
  # but with the overrides we need to build the `example-static-haskell-nix`
  # package.
  #
  # We don't override the `haskell` attribute directly (but instead add a
  # separate `my-haskell` attribute), since there are already a bunch of
  # Nixpkgs top-level derivations that use the `haskell` attibute, like
  # `cabal-install`, `stack`, etc.  We don't want to accidentally override
  # something they need, and cause them to break.
  my-haskell =
    let
      # A Nixpkgs Haskell package set override.
      myHaskellOverride = oldAttrs: {
        overrides =
          final.lib.composeExtensions
            (oldAttrs.overrides or (_: _: {}))
            # Our Nixpkgs Haskell package set overlay.
            #
            # This overlay just adds the `example-static-haskell-nix` package,
            # but you could additionally add/modify any transitive dependencies
            # you need.
            #
            # For instance, if you needed a different version of `lens` than
            # what already exists in the package set, you could add it here.
            (hfinal: hprev: {
              # The example Haskell package from this repo.
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

                  # This is somewhat complicated, but it is nice to
                  # explicitly ignore all files that don't affect the
                  # compilation of our Haskell package.
                  #
                  # This reduces the number of unnecessary rebuilds when
                  # unrelated files are changed.
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
                # This uses `callCabal2nix` to build our package with IFD, but
                # you could also just manually generate the package with
                # `cabal2nix` and call it with `hfinal.callPackage` if you
                # wanted to avoid IFD.
                hfinal.callCabal2nixWithOptions "example-static-haskell-nix" src extraCabal2nixOptions {};
            });
      };
    in
    # This set of overrides is necessary for overriding a single package set
    # within a deeply nested set of package sets.  See the Haskell section
    # in the Nixpkgs manual for more information:
    # https://nixos.org/manual/nixpkgs/stable/#haskell
    prev.haskell // {
      packages = prev.haskell.packages // {
        # Note that it is very important we use the `native-bignum` Haskell
        # sub-package-set.  The normal Haskell package sets don't work with
        # static compilation.
        native-bignum = prev.haskell.packages.native-bignum // {
          ${final.example-static-ghc-version} =
            prev.haskell.packages.native-bignum.${final.example-static-ghc-version}.override
              myHaskellOverride;
        };
      };
    };

  # This is a Nixpkgs Haskell package set where all Haskell libraries are linked
  # statically.  Any Haskell executables will be fully statically-linked.
  #
  # See the note above for why it is important to use a GHC from the
  # native-bignum sub-package-set.
  example-static-haskell-pkg-set = final.pkgsStatic.my-haskell.packages.native-bignum.${final.example-static-ghc-version};

  # This is our `example-static-haskell-nix` Haskell package fully statically linked.
  example-static = final.example-static-haskell-pkg-set.example-static-haskell-nix;

  # This is almost the same as `example-static` above, but it doesn't build the
  # library component of the package, only the executable.  It also does a few
  # other nice things, like disabling the profiling builds.
  #
  # For end-users that just want to use your executable, this is almost always
  # what they want.
  example-static-just-exe =
    final.lib.pipe
      final.example-static
      [ final.haskell.lib.justStaticExecutables
      ];
}

