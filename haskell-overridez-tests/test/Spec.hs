{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text         as Text
import           Test.Hspec

import           Nix.Paths
import           Turtle

import           Test.MkProjectDir (mkProjectDir)

main :: IO ()
main = hspec $ do
  describe "the haskell-overridez executable" $ do
    it "should be available" $ do
      procs "haskell-overridez" ["-l"] empty `shouldReturn` ()

  describe "the template directory" $ do
    it "should contain a cabal file" $ do
      mkProjectDir `shouldReturn` True

  describe "the nix-instantiate executable" $ do
    it "should be available" $ do
      procs (Text.pack nixInstantiate) ["--parse", "-E", "1 + 2"] empty `shouldReturn` ()
