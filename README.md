# Nix for Statically-linked Haskell Binaries

This repository is a self-contained example showing how to compile
statically-linked Linux x86-64 Haskell binaries with Nix. Statically-linked
Linux binaries are able to run on almost any Linux system, since they don't
require dynamically-linked dependencies to be available at runtime.

This repository also contains an example GitHub Action that builds the
statically-linked Linux binary, and attaches it as an asset whenever you
create a new GitHub Release.

This README explains the relationship between all the files in this repo.  All
of the individual files contain good documentation, so you should be able to
dive right in to the code if you'd like.

## Nix Files

Most users will be mainly interested in the Nix files in this repo.  The Nix
files work towards producing a derivation to build an x86-64 Linux Haskell
executable that is fully statically-linked.

Here's how to build the Haskell executable in this repo with Nix:

```console
$ nix-build
...
/nix/store/427pzjsrirf5asa80ihr6b1xra2gfb4i-example-static-haskell-nix-static-x86_64-unknown-linux-musl-0.1.0.0
```

You can confirm the produced executable is fully statically-linked:

```console
$ file ./result/bin/example
./result/bin/example: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, stripped
```

You can try to run it (it just prints the README to the console):

```console
$ ./example
...
```

You should be able to copy this executable to any x86-64 Linux system and run it
without problems.

Here's an overview of the Nix files:

- [`nix/overlay.nix`](./nix/overlay.nix):

    This file contains a Nixpkgs overlay.  This overlay adds a few attributes,
    including a Nixpkgs Haskell package set, which is setup for full
    static-linking.  It also contains an attribute referencing the Haskell
    package in this repo.

    This file is the most interesting, and contains the heart of the example in
    this repo.

- [`nix/default.nix`](./nix/default.nix):

    This file defines a Nixpkgs checkout, and returns it after applying the
    above overlay.

- [`default.nix`](./default.nix):

    This file returns the derivation for our statically-linked Haskell package
    from the above Nixpkgs package set.

## GitHub Action for Uploading Executable as Release Asset

This repo defines a single GitHub Action.  This GitHub Action contains a few
things, with the most interesting being a step that uploads the
statically-linked binary as an asset whenever you create a new GitHub Release.

You can see an example of what this looks like in from the
[release page](https://github.com/cdepillabout/example-static-haskell-nix/releases/tag/v0.1.0.0).

Here's an example how you could download the executable to your own system:

```console
$ curl -L https://github.com/cdepillabout/example-static-haskell-nix/releases/download/v0.1.0.0/example-static-linux-x86_64 > example
$ chmod +x ./example
```

You can confirm it is fully statically-linked:

```console
$ file ./example
./example: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, stripped
```

You can try to run it (it just prints out the beginning of the README to the console):

```
$ ./example
...
```

Here's where you can find the GitHub Action:

- [`.github/workflows/ci.yaml`](./.github/workflows/ci.yaml)

## Haskell Package

This repo also contains a Haskell package.  This is mostly uninteresting,
except for the note in [`src/ExampleStatic.hs`](./src/ExampleStatic.hs).

Here are the files relevant relevant to the Haskell package:

- [`example-static-haskell-nix.cabal`](./example-static-haskell-nix.cabal)
- [`src/ExampleStatic.hs`](./src/ExampleStatic.hs)
- [`app/Main.hs`](./app/Main.hs)
