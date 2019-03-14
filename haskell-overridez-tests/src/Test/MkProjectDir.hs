{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}
module Test.MkProjectDir
  ( -- * functions
  checkDerivation
  , mkProjectDir
  , mkProjectDir'
  , mkDerivation
  )
where

import qualified Control.Foldl                 as Foldl
import           Control.Monad.Managed         (MonadManaged)
import qualified Data.Attoparsec.Text    as Text
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Nix.Derivation                (parseDerivation, Derivation)
import           Nix.Paths                     (nixInstantiate)
import           Turtle
import qualified Turtle                        (FilePath)

import           Paths_haskell_overridez_tests


-- | Creates a project dir based on the template test dir.
mkProjectDir :: IO Bool
mkProjectDir = do
  z <- decodeString <$> getDataFileName "fixtures/template/project.cabal"
  testfile z


-- | mkDerivation obtains a derivation for the given name from a copy of the
-- template directory after running an action in it.
mkDerivation :: MonadManaged m => Text -> IO () -> m Derivation
mkDerivation name action = do
  dir <- mkProjectDir' name
  pushd dir
  liftIO $ do
    action
    runNixInstantiate name dir


-- | checkDerivation determines if the derivation created from a copy of the
-- template directory matches a filter.
checkDerivation :: MonadManaged m => (Derivation -> Bool) -> Text -> IO () -> m Bool
checkDerivation checkf name action = checkf <$> mkDerivation name action


runNixInstantiate :: Text -> Turtle.FilePath -> IO Derivation
runNixInstantiate name dir = do
  let cmd = inproc nixInstantiate' ["-A", name, (format fp dir)] empty
  fold (lineToText <$> cmd) Foldl.head >>= \case
    Nothing -> error $ "No derivation computed for " ++ (Text.unpack name)
    Just drvPath -> loadDerivation $ fromText drvPath


loadDerivation :: Turtle.FilePath -> IO Derivation
loadDerivation drvPath = do
  let strPath = Text.unpack $ format fp drvPath
  drv <- Text.readFile strPath
  case (Text.parse parseDerivation drv) of
    Text.Done _ z -> pure z
    _ -> error $ "failed to load a derivation from " ++ strPath


-- | Creates a project dir containing a default.nix based on the template test dir.
mkProjectDir' :: MonadManaged m => Text -> m Turtle.FilePath
mkProjectDir' name = do
  let cabal = fromText $ name <> ".cabal"
      nix = fromText $ name <> ".nix"
      projectDir = fromText name
  dir <- mktempdir "/tmp" name
  liftIO $ do
    void $ cpTemplateFile dir "LICENSE"
    mvPnameTemplate name cabal dir "project.cabal" >>= orUseAlt (projectDir </> cabal)
    mvPnameTemplate name nix dir "project.nix" >>= orUseAlt (projectDir </> nix)
    cpPnameTemplate name dir "default.nix" >>= orUseAlt (projectDir </> "default.nix")
    void $ cpProjectFile dir "lib.nix"
    pure dir


orUseAlt :: Turtle.FilePath -> Turtle.FilePath -> IO ()
orUseAlt alt dst = do
  let altPath = encodeString $ "fixtures" </> alt
  fullAltPath <- decodeString <$> getDataFileName altPath
  hasReplacement <- testfile fullAltPath
  when (hasReplacement) $ cp fullAltPath dst


-- | Copy a fixture template file to a new directory.
cpTemplateFile :: Turtle.FilePath -> Prelude.FilePath -> IO Turtle.FilePath
cpTemplateFile dir name = cpProjectFile dir ("fixtures/template/" ++ name)


-- | Copy a fixture template file to a new directory.
cpProjectFile :: Turtle.FilePath -> Prelude.FilePath -> IO Turtle.FilePath
cpProjectFile dir name = do
  raw <- decodeString <$> (getDataFileName name)
  let p = dir </> (filename raw)
  cp raw p
  pure p


-- | Copy a template file replacing the project name.
cpPnameTemplate :: Text -> Turtle.FilePath -> Prelude.FilePath -> IO Turtle.FilePath
cpPnameTemplate t = sedMvTemplateFile filename (replacePname t)


-- | Copy a template file replacing the project name.
mvPnameTemplate
  :: Text
  -> Turtle.FilePath
  -> Turtle.FilePath
  -> Prelude.FilePath
  -> IO Turtle.FilePath
mvPnameTemplate t newName = sedMvTemplateFile (const newName) (replacePname t)


-- | Copy a template file, using a renamer function to determine its final name.
sedMvTemplateFile
  :: (Turtle.FilePath -> Turtle.FilePath)
  -> (Shell Line -> Shell Line)
  -> Turtle.FilePath
  -> Prelude.FilePath
  -> IO Turtle.FilePath
sedMvTemplateFile rename updates dir name = do
  raw <- decodeString <$> (getDataFileName $ "fixtures/template/" ++ name)
  let p = dir </> rename raw
  output p $ updates $ input raw
  pure p


replacePname :: Text -> Shell Line -> Shell Line
replacePname t = sed $ (fmap (const t) $ Turtle.text "purojekuto-no-namae")


nixInstantiate' :: Text
nixInstantiate' = Text.pack nixInstantiate
