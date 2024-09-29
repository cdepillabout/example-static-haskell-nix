# This file defines a Nixpkgs package set, and applies ./overlay.nix as an
# overlay.
#
# You can jump into a repl session with this Nixpkgs loaded by doing:
#
# $ nix repl ./nix

{...}:

let
  nixpkgs-src = builtins.fetchTarball {
    # haskell-updates as of 2024-09-26
    #
    # I suggest using the `haskell-updates` branch, instead of `nixpkgs-unstable`
    # or a stable branch because the `haskell-updates` branch is more likely to
    # give you a GHC for static linking from the NixOS cache.
    #
    # The `haskell-updates` branch has a Hydra jobset that is separate from
    # branches like `nixos-unstable` and `nixpkgs-unstable`.  The `haskell-updates`
    # Hydra jobset includes jobs that build GHCs for static linking, as well as
    # a few common Haskell libraries.  This means that you should be able to
    # download these GHCs from the shared NixOS cache, instead of having to
    # build them yourself.
    #
    # One problem is that the `haskell-updates` branch is often broken, so you
    # have to make sure to pick a commit where everything is working.  I
    # recommend looking at old PRs merging `haskell-updates` into `master`, and
    # picking a commit immediately before the branch is merged.  In almost all
    # cases, these commits are fully working, as well as having all jobs cached
    # by the NixOS cache.
    #
    # Here's a URL you can use to find these PRs in the Nixpkgs repo:
    #
    # https://github.com/nixos/nixpkgs/pulls?q=is%3Apr+head%3Ahaskell-updates+is%3Aclosed+base%3Amaster+%22haskellPackages%3A+update%22+
    #
    # Also, keep in mind that full static linking currently only works with
    # GHC-9.4, while the main GHC in Nixpkgs is 9.6.  This means that if you
    # have a complex project that uses many Haskell dependencies, you may have
    # an easier time going back to an earlier Nixpkgs where the main GHC was
    # still 9.4, since more of the Haskell package set will be working by
    # default.  See the note about this in ./overlays.nix.
    url = "https://github.com/NixOS/nixpkgs/archive/b6b063bdc265990fb87781682da974578b16443c.tar.gz";
    sha256 = "1q28fpgfhlix0rkpdvqxy8vnyv1hiz2wnkh66pxh49za1hjjimdg";

    # nixpkgs-unstable as of 2024-09-26
    #
    # If you want to build statically-linked Haskell binaries, and you have the
    # option, I'd recommend using a commit from the `haskell-updates` branch
    # instead of `nixpkgs-unstable` given the reason in the comment above.
    #
    # If you don't have the option, and you need to track a branch like
    # `nixpkgs-unstable`, you're unlikely to have problems, but you will likely
    # have to build GHC from source.
    #url = "https://github.com/NixOS/nixpkgs/archive/965289e5e07243f1cde3212d8bcaf726d36c5c46.tar.gz";
    #sha256 = "0kmnaamjhqj0c2bwhj22dxx4dkpg5j6a4wn02bkkc15ah1cjblrs";
  };

  my-overlay = import ./overlay.nix;

in

import nixpkgs-src { overlays = [ my-overlay ]; }
