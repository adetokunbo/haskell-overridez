{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Turtle

import           System.Exit       (ExitCode)
import           Test.Derivation   (Dependency, asCabalUri, hasDeps, mkDep,
                                    mkNameOnly)
import           Test.MkProjectDir (checkDerivation, cpWithAltDir, checkDerivation')


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

      it "should create an overlay from a github subdir repo" $ do
        let
          name = "kyuubanme-no-purojekuto"
          subpath = "logging-effect-extra-handler"
          setup = hoz [leExtraUri, "--", "--subpath", subpath]
          checkDrv = hasDeps $ map mkNameOnly [subpath]
          theTest = with (checkDerivation checkDrv name setup) pure
        theTest `shouldReturn` True

    context "using git json generated via nix-prefetch-git" $ do
      it "should create an overlay from github repo names" $ do
        let
          -- can't use -g with optparse-applicative or foldl, this makes
          -- cabal2nix infinitely recurse when converting json to nix-expr
          setup = do
            hoz [ "--flag-override", "DontCheck", optParseUri]
            hoz [ "--flag-override", "DoJailbreak", "-g"
                , "Gabriel439/Haskell-Turtle-Library"
                ]
            hoz [ "--flag-override", "DoJailbreak", foldUri]
          checkDrv = hasDeps $ map mkNameOnly ["turtle", "foldl"]
          theTest = with (checkDerivation checkDrv "rokubanme-no-purojekuto" setup) pure
        theTest `shouldReturn` True

    context "using fetched config" $ do
      it "should create an overlay from config in a repo main folder" $ do
        let
          name = "ichibanme-no-yagai-purojekuto"
          nixDir = "./nix/github.com/adetokunbo/example-fetched-haskell-overridez"
          setup = hoz ["fetch", exGithubUri]
          checkDrv = hasDeps $ map mkNameOnly ["turtle", "foldl"]
          copyRule = cpWithAltDir name nixDir
          theTest = with (checkDerivation' checkDrv copyRule setup) pure
        theTest `shouldReturn` True

      it "should create an overlay from config in a repo sub directory" $ do
        let
          name = "gobanme-no-yagai-purojekuto"
          nixDir = "./nix/github.com/adetokunbo/example-fetched-haskell-overridez"
                   </> "subdir-root" </> "a-subdir"
          setup = hoz ["fetch", "--subpath", "subdir-root/a-subdir", exGithubUri]
          checkDrv = hasDeps $ map mkNameOnly ["turtle", "foldl"]
          copyRule = cpWithAltDir name nixDir
          theTest = with (checkDerivation' checkDrv copyRule setup) pure
        theTest `shouldReturn` True

    context "saving to an alternate output directory" $ do
      it "should create an overlay from github https:// urls" $ do
        let
          name = "hachibanme-no-purojekuto"
          setup altDir = do
            hoz' altDir ["--flag-override", "DontCheck", optParseUri]
            hoz' altDir ["--flag-override", "DoJailbreak", turtleUri]
            hoz' altDir ["--flag-override", "DoJailbreak", foldUri]
          checkDrv = hasDeps $ map mkNameOnly ["turtle", "foldl"]
          theTest = flip with pure $ do
            altDir <- mktempdir "/tmp" $ name <> "-alt"
            let copyRule = cpWithAltDir name $ altDir </> "nix"
            checkDerivation' checkDrv copyRule $ setup altDir
        theTest `shouldReturn` True

    context "when the haskell-overridez functions are not used " $ do
      it "should fail to create an overlay" $ do
        let
          setup = do
            hoz ["--flag-override", "DontCheck", asCabalUri optParseDep]
            hoz ["--flag-override", "DoJailbreak", asCabalUri foldlDep]
            hoz ["--flag-override", "DoJailbreak", asCabalUri turtleDep]
          checkDrv = hasDeps [foldlDep, turtleDep]
          theTest = with (checkDerivation checkDrv "nanabanme-no-purojekuto" setup) pure
        theTest `shouldThrow` anyExitcode


hoz :: MonadIO io => [Text] -> io ()
hoz args = procs "haskell-overridez" args empty


hoz' :: MonadIO io => Turtle.FilePath -> [Text] -> io ()
hoz' altDir args = hoz $ args <> ["-o", format fp altDir]


turtleUri, foldUri, optParseUri, leExtraUri, exGithubUri :: Text
turtleUri   = "https://github.com/Gabriel439/Haskell-Turtle-Library"
foldUri     = "https://github.com/Gabriel439/Haskell-Foldl-Library"
optParseUri = "https://github.com/pcapriotti/optparse-applicative"
leExtraUri  = "https://github.com/jship/logging-effect-extra"
exGithubUri = "https://github.com/adetokunbo/example-fetched-haskell-overridez"


turtleDep, foldlDep, optParseDep :: Dependency
turtleDep = mkDep "turtle" "1.5.12"
foldlDep = mkDep "foldl" "1.4.5"
optParseDep = mkDep "optparse-applicative" "0.14.2.0"


anyExitcode :: Selector ExitCode
anyExitcode = const True
