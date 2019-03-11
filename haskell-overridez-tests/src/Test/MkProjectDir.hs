{-# LANGUAGE OverloadedStrings #-}
module Test.MkProjectDir
  ( mkProjectDir'
  , mkProjectDir
  )
where

import           Data.List               (foldl')
import           Data.List.NonEmpty      (toList)
import           Turtle
import qualified Turtle                  (FilePath)
import qualified Control.Foldl           as Foldl
import           Control.Monad.Managed   (MonadManaged)

import           Paths_haskell_overridez_tests


-- | Creates a project dir based on the template test dir.
mkProjectDir :: IO Bool
mkProjectDir = do
  cabalFile <- decodeString <$> getDataFileName "fixtures/template/project.cabal"
  testfile cabalFile


-- | Creates a project dir based on the template test dir.
mkProjectDir' :: MonadManaged m => Text -> m Turtle.FilePath
mkProjectDir' name = do
  let cabal = fromText $ name <> ".cabal"
      nix = fromText $ name <> ".nix"
      projectDir = fromText name
  dir <- mktempdir "/tmp" name
  liftIO $ do
    void $ cpTemplateFile dir "LICENSE"
    mvPnameTemplate cabal dir "project.cabal" >>= orUseAlt (projectDir </> cabal)
    mvPnameTemplate nix dir "project.nix" >>= orUseAlt (projectDir </> nix)
    cpPnameTemplate dir "default.nix" >>= orUseAlt (projectDir </> "default.nix")
    void $ cpProjectFile dir "lib.nix"
    pure dir

  -- "nix-instantiate" ["-A", name, "default.nix"], get stdout
  -- ""


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
  let p = dir </> (basename raw)
  cp raw p
  pure p


-- | Copy a template file replacing the project name.
cpPnameTemplate :: Turtle.FilePath -> Prelude.FilePath -> IO Turtle.FilePath
cpPnameTemplate = sedMvTemplateFile basename [replacePname]


-- | Copy a template file replacing the project name.
mvPnameTemplate
  :: Turtle.FilePath
  -> Turtle.FilePath
  -> Prelude.FilePath
  -> IO Turtle.FilePath
mvPnameTemplate newName = sedMvTemplateFile (const newName) [replacePname]


-- | Copy a template file, using a renamer function to determine its final name.
sedMvTemplateFile
  :: (Turtle.FilePath -> Turtle.FilePath)
  -> [Text -> Text]
  -> Turtle.FilePath
  -> Prelude.FilePath
  -> IO Turtle.FilePath
sedMvTemplateFile rename updates dir name = do
  raw <- decodeString <$> (getDataFileName $ "fixtures/template/" ++ name)
  let p = dir </> rename raw
      updateLines = replaceAll updates . lineToText
  initial <- fold (input raw) Foldl.list
  output p $ shTextToLines $ select $ map updateLines $ initial
  pure p


replaceAll :: [a -> a] -> a -> a
replaceAll rs acc = foldl' apply' acc rs
  where apply' a func = func a


replacePname :: Text -> Text
replacePname pname = fmap (const pname) Turtle.text "purojekuto-no-namae"


shTextToLines :: Shell Text -> Shell Line
shTextToLines = join . fmap (select . toList . textToLines)
