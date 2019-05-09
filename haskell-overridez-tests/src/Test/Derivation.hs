{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}
module Test.Derivation
  ( -- * data types
    Dependency

    -- * functions
  , asCabalUri
  , hasDeps
  , mkNameOnly
  , mkDep

    -- * re-exports
  , module Nix.Derivation
  )

where

import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS (FilePath, toText)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (FilePath)

import           Nix.Derivation            (Derivation (..))


-- | @Dependency@ is a package that may be a key in 'Derivation' 'inputDrvs'.
data Dependency = Dependency
  { depName    :: !Text
  , depVersion :: !(Maybe Text)
  } deriving (Show, Eq, Generic)


-- | construct a dependency with name only
mkNameOnly :: Text -> Dependency
mkNameOnly n = Dependency n Nothing


-- | construct a dependency with name and version
mkDep :: Text -> Text -> Dependency
mkDep n v = Dependency n $ Just v


-- | Given a 'Derivation' determine if all of the given dependencies are present.
hasDeps :: [Dependency] -> Derivation -> Bool
hasDeps ds drv = all hasMatch ds
  where
    hasMatch d = any (depMatches d) drvDeps
    drvDeps = asInputDeps drv


depMatches :: Dependency -> Dependency -> Bool
depMatches d1@(Dependency _ (Just _)) d2@(Dependency _ (Just _)) = d1 == d2
depMatches (Dependency n1 _) (Dependency n2 _)                   = n1 == n2


asInputDeps :: Derivation -> [Dependency]
asInputDeps = map (drvToDep . fst) . Map.toAscList . inputDrvs


drvToDep :: FilePath -> Dependency
drvToDep = toDep . devName
  where
    -- N.B. this assumes that the path name is:
    -- /nix/store/${32_CHARACTER_HASH}-${NAME}.drv
    devName = Text.dropEnd 4 . Text.drop 44 . pathToText
    pathToText = either id id . toText
    toDep t = case (Text.breakOnEnd "-" t) of
      ("", name)      -> Dependency name Nothing
      (name, version) -> Dependency (Text.dropWhileEnd (== '-') name) $ Just version


-- | Render as dependency as the equivalent cabal uri.
asCabalUri :: Dependency -> Text
asCabalUri d = "cabal://" <> depName d <> "-" <> (maybe "" id $ depVersion d)
