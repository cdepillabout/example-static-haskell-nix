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
readme = $(embedFile "../README.md")
