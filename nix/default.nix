
{...}:

let
  nixpkgs-src = builtins.fetchTarball {
    # nixpkgs-unstable as of 2024-09-26
    #url = "https://github.com/NixOS/nixpkgs/archive/965289e5e07243f1cde3212d8bcaf726d36c5c46.tar.gz";
    #sha256 = "19j550srrsmsfzz0arfva1n13kjdz5yiz3x2ss3mgpaxacny7iaa";

    # haskell-updates as of 2024-09-26
    #
    # TODO: explain why to use this branch
    url = "https://github.com/NixOS/nixpkgs/archive/b6b063bdc265990fb87781682da974578b16443c.tar.gz";
    sha256 = "19j550srrsmsfzz0arfva1n13kjdz5yiz3x2ss3mgpaxacny7iaa";
  };

  my-overlay = import ./overlay.nix;

in

import nixpkgs-src { overlays = [ my-overlay ]; }