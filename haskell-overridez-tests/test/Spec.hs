{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Turtle

import           Test.Derivation   (Dependency, asCabalUri, hasDeps, mkDep,
                                    mkNameOnly)
import           Test.MkProjectDir (checkDerivation)


main :: IO ()
main = hspec $ do
  describe "haskell-overridez" $ do
    context "using nix-exprs generated via cabal2nix" $ do
      it "should create an overlay from https urls" $ do
        let
          setup = do
            hoz ["--flag-override", "DontCheck", optParseUri]
            hoz ["--flag-override", "DoJailbreak", turtleUri]
            hoz ["--flag-override", "DoJailbreak", foldUri]
          checkDrv = hasDeps $ map mkNameOnly ["turtle", "foldl"]
          theTest = with (checkDerivation checkDrv "gobanme-no-purojekuto" setup) pure
        theTest `shouldReturn` True

      it "should create an overlay from cabal:// urls" $ do
        let
          setup = do
            hoz ["--flag-override", "DontCheck", asCabalUri optParseDep]
            hoz ["--flag-override", "DoJailbreak", asCabalUri foldlDep]
            hoz ["--flag-override", "DoJailbreak", asCabalUri turtleDep]
          checkDrv = hasDeps [foldlDep, turtleDep]
          theTest = with (checkDerivation checkDrv "yonbanme-no-purojekuto" setup) pure
        theTest `shouldReturn` True

    context "using git json generated via nix-prefetch-git" $ do
      it "should create an overlay from github repo names" $ do
        let
          -- can't use -g with optparse-applicative or foldl, this makes
          -- cabal2nix infinitely recurse when converting json to nix-expr
          setup = do
            hoz ["--flag-override", "DontCheck", optParseUri]
            hoz [ "--flag-override", "DoJailbreak", "-g"
                , "Gabriel439/Haskell-Turtle-Library"
                ]
            hoz ["--flag-override", "DoJailbreak", foldUri]
          checkDrv = hasDeps $ map mkNameOnly ["turtle", "foldl"]
          theTest = with (checkDerivation checkDrv "rokubanme-no-purojekuto" setup) pure
        theTest `shouldReturn` True


hoz :: MonadIO io => [Text] -> io ()
hoz args = procs "haskell-overridez" args empty


turtleUri, foldUri, optParseUri :: Text
turtleUri   = "https://github.com/Gabriel439/Haskell-Turtle-Library"
foldUri     = "https://github.com/Gabriel439/Haskell-Foldl-Library"
optParseUri = "https://github.com/pcapriotti/optparse-applicative"


turtleDep, foldlDep, optParseDep :: Dependency
turtleDep = mkDep "turtle" "1.5.12"
foldlDep = mkDep "foldl" "1.4.5"
optParseDep = mkDep "optparse-applicative" "0.14.2.0"
