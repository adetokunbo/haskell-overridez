{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Turtle

import           Test.Derivation   (hasDeps, mkNameOnly)
import           Test.MkProjectDir (checkDerivation)


main :: IO ()
main = hspec $ do
  describe "haskell-overridez" $ do
    context "using nix-exprs generated via github https urls" $ do
      it "should create an overlay" $ do
        let
          doSetup = do
            hoz ["--flag-override", "DontCheck", optParseUri]
            hoz ["--flag-override", "DoJailbreak", turtleUri]
            hoz ["--flag-override", "DoJailbreak", foldUri]
          checkDrv = hasDeps $ map mkNameOnly ["turtle", "foldl"]
          theTest = with (checkDerivation checkDrv "gobanme-no-purojekuto" doSetup) pure

        theTest `shouldReturn` True


hoz :: MonadIO io => [Text] -> io ()
hoz args = procs "haskell-overridez" args empty


turtleUri, foldUri, optParseUri :: Text
turtleUri   = "https://github.com/Gabriel439/Haskell-Turtle-Library"
foldUri     = "https://github.com/Gabriel439/Haskell-Foldl-Library"
optParseUri = "https://github.com/pcapriotti/optparse-applicative"
