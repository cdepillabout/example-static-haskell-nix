{-# LANGUAGE TemplateHaskell #-}

module ExampleStatic where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.FileEmbed (embedFile)

defaultMain :: IO ()
defaultMain = do
  putStrLn "Here's the README from this project:\n"
  ByteString.putStr readme

readme :: ByteString
readme =
  -- Note that starting in GHC-9.6, there have been quite a few problems with
  -- building fully statically-linked Haskell binaries in Nixpkgs.  See the
  -- note about this in ../nix/overlay.nix.
  --
  -- When testing whether or not your static-linking setup works, you definitely
  -- want to confirm that it is possible to build libraries that use
  -- TemplateHaskell.  If you're not careful, it is possible to build a GHC that
  -- can do static-linking, but ONLY when no libraries use TemplateHaskell.
  --
  -- Most complex, serious Haskell projects end up pulling in at least one
  -- transitive dependency that uses TemplateHaskell, so it is good to confirm
  -- it is possible to use TemplateHaskell with your static-linking setup.
  --
  -- The following uses TemplateHaskell with a function from file-embed library
  -- to make sure this works.
  $(embedFile "README.md")
