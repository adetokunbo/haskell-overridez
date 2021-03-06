{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Main (main) where

import           Control.Monad                (join, void)
import           Control.Monad.Catch          (MonadMask, bracket)
import           Data.Bifunctor               (first)

import           Data.Function                (on)
import           Data.List.NonEmpty           (toList)
import           Data.Maybe                   (catMaybes, listToMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Version                 (showVersion)
import           GHC.Generics
import           Paths_haskell_overridez      (version)
import           Prelude                      hiding (FilePath)

import           Control.Monad.Managed        (managed, runManaged)
import qualified Data.Aeson                   as Aeson
import           Data.Aeson.Casing
import           Filesystem.Path              (directory, filename)
import           Filesystem.Path.CurrentOS    hiding (empty)
import           NeatInterpolation
import           Network.URI                  (URI (..), parseURI, uriRegName)
import qualified Options.Applicative          as Options
import           Turtle                       hiding (text)
import qualified Turtle                       (text)

-- Imports that allow the implementation of the json methods
import           Control.Foldl                (Fold, FoldM (..))
import qualified Control.Foldl                as Foldl
import qualified Data.Aeson.Parser            as Aeson
import qualified Data.Attoparsec.ByteString   as A
import qualified Data.ByteString              as ByteString
import qualified Data.Text.Encoding           as Text

-- Accessing information in cabal files
import           Distribution.Pretty          (Pretty, prettyShow)
import           Distribution.Text            (simpleParse)
import qualified Distribution.Text            as DistText
import           Distribution.Types.PackageId (PackageIdentifier (..))
import           Distribution.Types.Version   (Version, nullVersion)

data Options
  = Fetch
    { _target   :: Text           -- ^ the git repo to download from
    , _subpath  :: Maybe FilePath -- ^ the directory in the repo containing the config
    , _revision :: Maybe Text     -- ^ the target revision
    , _hash     :: Maybe Text     -- ^ the hash at the target revision
    }
  | Delete Text  -- ^ Specifies an override to be deleted
  | List         -- ^ List the current overrides
  | Initialize   -- ^ Initialize (or re-initialize) a project using overrides
  | Add          -- ^ Add an override
    { _asJson    :: Bool       -- ^ Save it as github json
    , _target    :: Text       -- ^ Name of the target override
    , _args      :: [Text]     -- ^ Additional args for the downloading the target
    , _cabalOpts :: [CabalOpt] -- ^ Cabal option override
    } deriving Show


cmdlineWithVersion :: Options.Parser (Options, Maybe FilePath)
cmdlineWithVersion =
  Options.infoOption
  (showVersion version)
  (Options.long "version"
    <> Options.short 'v'
    <> Options.help "Show the version of haskell-overridez")
  <*> cmdlineParser

cmdlineParser :: Parser (Options, Maybe FilePath)
cmdlineParser
  = (,)
  <$>
  ((subcommand "fetch" "Fetches overrides from a git repo"
    (Fetch
      <$> argText "target" "The url hosting the shared config."
      <*> optional (optPath "subpath" 'd'
        "the subdirectory of the git repo that contains the config")
      <*> optional (argText "revision" "The git revision to download from")
      <*> optional (argText "hash"
                    "When specified, the hash is used to verify the contents of the repo")))
   <|> (Delete <$> optText "delete" 'd' "Deletes the overrides for a package.")
   <|> Options.flag' List (
      Options.long "list"
        <> Options.short 'l'
        <> Options.help "List the overridden packages in the project")
   <|> Options.flag' Initialize
    (Options.long "initialize"
     <> Options.short 'i'
     <> Options.help
      "Reinitialize the project. This adds the file \
      \ `nix/haskell-overridez.nix` to the project if it is not present\
      \ or updates it to reflect the current version of\
      \ haskell-overiddez. This file simplifies the\
      \ import of the haskell-overridez nixpkgs functions")
  <|>
    (Add
     <$> switch "github" 'g'
     "Save github json describing the package git repo instead of a nix expr.\
     \ TARGET is required to be <username/project>. If an additional\
     \ argument is given, it is treated as the git revision to be downloaded"
     <*> argText "target"
     "The target package. Unless the --github flag is specified, this\
     \ is any target url that the cabal2nix command understand."
     <*> many (argText "additional args"
                "Unless --github is specified, these are additional args\
                \ to pass on to cabal2nix; use -- to precede any options"))
     <*> many (Options.option Options.auto (
                  Options.long "flag-override"
                  <> Options.metavar "FLAG_OVERRIDES"
                  <> Options.help
                  "Cabal flag overrides to include in the package override\
                  \ specification  "))
  )
  <*>
   optional (
      optPath "project-dir" 'o'
        "the target directory. It defaults to the current working directory.")

-- | Represents the information obtained from running nix-prefetch-git.
data PrefetchInfo =
  PrefetchInfo
  { _pfUrl    :: Text
  , _pfRev    :: Text
  , _pfSha256 :: Text
  } deriving (Show, Generic)

instance Aeson.FromJSON PrefetchInfo where
  parseJSON = Aeson.genericParseJSON $ aesonPrefix snakeCase

mainSummary :: Description
mainSummary =
  "Adds override files to the nix subdirectory of a project\n\n\
  \The files are either the 'prefetch' json or the nix expression\n\
  \output of cabal2nix describing the target haskell package.\  \\n\n\
  \These files are used by the functions of the accompanying\n\
  \nix-expr library to create an override function that\n\
  \combines all the specified overrides."

main :: IO ()
main = options mainSummary cmdlineWithVersion >>= runCmd

runCmd :: (Options, Maybe FilePath) -> IO ()
runCmd (List, wd) = inOtherCwd wd $ listOverrides
runCmd ((Delete p), wd) = inOtherCwd wd $ removePackage p
runCmd (Initialize, wd) = inOtherCwd wd $ initializeProject True

runCmd (Add {_asJson, _target, _args, _cabalOpts}, wd)
  | _asJson = inOtherCwd wd $ saveGitJson _target _args _cabalOpts
  | otherwise = case parsePkgId _target of
      Nothing    -> inOtherCwd wd $ saveNixExpr _target _args _cabalOpts
      Just pkgId -> inOtherCwd wd $ saveFromPkgId pkgId _args _cabalOpts

runCmd (Fetch {_target, _subpath, _revision, _hash}, wd) =
  let
    fpLine = maybe "" id . textToLine . format fp
    matchRoot = Turtle.text . (flip (<>) "/") . format fp
    dropRoot dir = sed (matchRoot dir *> return "")
    grepOverridez dir = dropRoot dir
      $ grep allOverridezPat $ fpLine <$> lstree dir
    fromTextStr = fromText . Text.pack
    fetchPathOf aUrl
      | uriScheme aUrl == "file:" = fileURIPath aUrl
      | otherwise = authPath aUrl <$> uriAuthority aUrl
    fileURIPath aUrl =
      Just $ "nix" </> "localhost" </> (filename $ fromTextStr $ uriPath aUrl)
    authPath aUrl a = "nix" </>
                   (fromTextStr $ uriRegName a) </>
                   (fromTextStr $ tail $ uriPath aUrl)
    -- appendSubpath dir = Just $ maybe dir ((</>) dir) _subpath
  in
    case ((parseURI $ Text.unpack _target) >>= fetchPathOf) of
      Nothing -> die $ format ("not a valid target uri: "%s%"") _target
      (Just dst) -> do
        cwd <- pwd
        let dst' = (maybe cwd id wd) </> dst
        sh $ do
          tmp <- using (mktempdir "/tmp" "hozfetch")
          sh $ saveRemoteRepo tmp _target _revision _hash
          let src = maybe tmp (flip (</>) tmp) _subpath
          fold (grepOverridez src) Foldl.list >>= \case
            [] -> die $ format ("no config found at "%s%"") _target
            zs -> cpTo dst' src $ (fromText . lineToText) <$> select zs

setAllHashesMsg :: Text -> Text
setAllHashesMsg name =
  format ("\nCannot handle "%s%"\n") name
  <> format (" Please set the environment variable "%s%"") allHashesEnv
  <> ".\n This should be the path to a nix data derivation containing\
     \ the hashes and cabal files for all packages on hackage.\n Its\
     \ contents are required to correctly resolve cabal:// urls. \
     \ When haskell-overridez is installed using nix, this environment\
     \ variable is set automatically.\n"

-- | Save a nix expression file for a package specified by its 'PackageIdentifier'.
saveFromPkgId :: PackageIdentifier -> [Text] -> [CabalOpt] -> IO ()
saveFromPkgId pkgId extraArgs cabalOpts =  do
  let whenAllHashes = flip $ maybe $ die $ setAllHashesMsg $ prettyShow' pkgId
  hs <- lookupEnv allHashesEnv
  whenAllHashes hs $ \allHashes' -> do
    pkg <- findPackage pkgId $ inproc "tar" ["tzvf", allHashes'] empty
    cwd <- pwd
    runManaged $ do
      untarDir <- using (mktempdir cwd "hoz-untar")
      cwd' <- managed (withOtherCwd $ Just untarDir)
      procs "tar" ["xvf", allHashes', pkg] empty

      -- needed because cabal2nix examines a cache in $HOME for local
      -- packages, when run from a nix-shell this breaks as $HOME is reset to
      -- a directory that does not exist
      export "HOME" $ format fp untarDir

      _ <- managed (withOtherCwd $ Just cwd')
      saveNixExpr (format fp untarDir <> "/" <> pkg) extraArgs cabalOpts


-- | Save a nix expression file in the current project by shelling out to
-- cabal2nix.
saveNixExpr :: MonadIO io => Text -> [Text] -> [CabalOpt] -> io ()
saveNixExpr url extraArgs cabalOpts = do
  let findPname = findNixField "pname"
      shCmd = lineToText <$> inproc "cabal2nix" ([url] <> extraArgs) empty
  (mbPackage, lines_) <- fold shCmd findPname
  case mbPackage of
    Nothing ->
      die $ format ("could not find a package name in the output of"%s%"") url
    (Just p) -> do
      let dstText = format ("nix/nix-expr/"%s%".nix") p
          dst = fromText dstText
      mktree (directory dst)
      output dst $ shTextToLines $ select lines_
      initializeProject False
      addConfiguredOptions cabalOpts $ unsafeTextToLine p
      cwd <- format fp <$> pwd
      printf ("saved nix-expr for "%s%" to "%s%" in "%s%"\n") p dstText cwd

-- | Save a package as a JSON descriptor of its github repository.
saveGitJson :: MonadIO io => Text -> [Text] -> [CabalOpt] -> io ()
saveGitJson userRepo [] cabalOpts = saveGitJson userRepo [""] cabalOpts
saveGitJson userRepo (revision : _xs) cabalOpts = do
  let target = format ("https://github.com/"%s%".git") userRepo
      just z = if z == "" then Nothing else Just z
      defPkgName = snd $ Text.breakOnEnd "/" userRepo
  liftIO $ runManaged $ do
    tmp <- using (mktempdir "/tmp" "hoz-json")
    sh $ saveRemoteRepo tmp target (just revision) Nothing
    pkgName <- pkgNameOrDef tmp defPkgName
    let dstText = format ("nix/git-json/"%s%".json") pkgName
        dst = fromText dstText
    mktree (directory dst)
    cp (tmp <> githubJsonPath) dst
    addConfiguredOptions cabalOpts $ unsafeTextToLine pkgName
    initializeProject False
    cwd <- format fp <$> pwd
    printf ("saved git json for "%s%" to "%s%" in "%s%"\n") pkgName dstText cwd

-- | Determine the package name from the contents of the project directory or
-- uses the provided default value.
pkgNameOrDef :: MonadIO io => FilePath -> Text -> io Text
pkgNameOrDef dir def = do
  fromDir <- do
    fromNix <- pkgNameFromNix dir
    fromCabal <- pkgNameFromCabal dir
    return $ fromNix <|> fromCabal
  case fromDir of
    Nothing -> do
      liftIO $ do
        echo "*warning* could not package name in a cabal or default.nix file"
        printf ("*warning* - defaulting to "%s%"\n") def
        echo "*warning* Files examined are:"
        view $ format fp <$> ls dir
      return def
    Just pkg -> return pkg

-- | Determine the package name by examining the default.nix file in the project
-- directory if that is present.
pkgNameFromNix :: MonadIO io => FilePath -> io (Maybe Text)
pkgNameFromNix projDir = do
  let nixPath =  projDir <> "nix/default.nix"
      findPname = findNixField "pname"
  liftIO $ testfile nixPath >>= \case
    False -> return Nothing
    True -> fst <$> fold (lineToText <$> input nixPath) findPname

-- | Determine the package name by examining the cabal file in the project
-- directory if that is present.
pkgNameFromCabal :: MonadIO io => FilePath -> io (Maybe Text)
pkgNameFromCabal projDir = do
  let fs = grepText (suffix ".cabal") (format fp <$> ls projDir)
      names = sed findName $ grep findName $ join $ (input . fromText) <$> fs
      findName = do
        void $ "name:" <|> "Name:"
        void $ spaces1
        name <- plus anyChar
        return name
  fold names Foldl.list >>= \case
    [z] -> return $ Just $ lineToText z
    _   -> return Nothing

-- | Initialize the current project.
initializeProject :: MonadIO io => Bool -> io ()
initializeProject always = do
  uninitialized <- testfile loaderPath
  when (always || not uninitialized) $ do
    hash <- single (inshell computeLoaderHashCmd empty)
    let nixExp = computeLoaderContent archiveUrl $ lineToText hash
    output loaderPath $ select $ textToLines nixExp

-- | Add any configured options for a package.
addConfiguredOptions :: MonadIO io => [CabalOpt] -> Line -> io ()
addConfiguredOptions cabalOpts pkgName =
  mapM_ (addLine pkgName . configPath) cabalOpts

-- | Remove the override configuration for a package from the current project.
removePackage :: Text -> IO ()
removePackage package = do
  let nixExpr = fromText $ "nix/nix-expr/" <> package <> ".nix"
      gitJson = fromText $ "nix/git-json/" <> package <> ".json"
      rmWhenPresent f' = testfile f' >>= (flip when $ rm f')
      rmFromFile = removeLine package
  rmWhenPresent nixExpr
  rmWhenPresent gitJson
  mapM_ (rmFromFile . configPath) [minBound .. maxBound]

-- | List the overrides configured in the current project.
listOverrides :: IO ()
listOverrides = do
  let dropExt ext = sedSuffix $ ext *> ""
      matchRoot = Turtle.text . (flip (<>) "/") . format fp
      dropRoot dir = sed (matchRoot dir *> return "")
      fpLine = maybe "" id . textToLine . format fp
      lsNoPrefix dir = dropRoot dir $ fpLine <$> ls dir
      list' title dir ext = do
        exists <- testdir dir
        when exists $ stdout $ title <|> "" <|> (dropExt ext $ lsNoPrefix dir)
  list' "Nix exprs" "nix/nix-expr" ".nix"
  stdout (return "")
  list' "Github json" "nix/git-json" ".json"

-- | Change the current working directory while performing an action.
--
-- Similar to 'inOtherCwd', but can be used with 'managed'
withOtherCwd
  :: (MonadIO io, MonadMask io)
  => Maybe FilePath
  -> (FilePath -> io a) -> io a
withOtherCwd Nothing    action = pwd >>= action
withOtherCwd (Just dst) action =
  let
    switchd = pwd >>= (\cwd -> cd dst >> showDir dst >> return cwd)
    showDir = printf ("working dir is now "%fp%"\n")
  in
    bracket (liftIO switchd) (liftIO . cd) action

-- | Change the current working directory while performing an action.
inOtherCwd :: (MonadIO io, MonadMask io) => Maybe FilePath -> io a -> io a
inOtherCwd mbDir action = withOtherCwd mbDir $ const action

saveRemoteRepo :: FilePath
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Shell Text
saveRemoteRepo dst target (Just rev) (Just hash) = do
  lineToText <$> inproc "nix-prefetch-git"
    [ "--quiet"
    , "--builder"
    , "--out"
    , format fp dst
    , target
    , rev
    , hash
    ] empty

saveRemoteRepo dst target (Just rev) Nothing = do
  let jsonOut = dst <> githubJsonPath
  output jsonOut $ inproc "nix-prefetch-git" [ target , rev ] empty
  info <- liftIO $ json $ lineToText <$> input jsonOut
  saveRemoteRepo dst target (Just $ _pfRev info) (Just $ _pfSha256 info)

saveRemoteRepo dst target _ _ =
  saveRemoteRepo dst target (Just "") Nothing


findPackage :: PackageIdentifier -> Shell Line -> IO Text
findPackage pkgId@PackageIdentifier{pkgName, pkgVersion} xs = do
  maxVersion <- fold withVersions $ Foldl.maximumBy (compare `on` snd)
  case maxVersion of
    Nothing           -> die $ missingErr $ prettyShow' pkgId
    Just (_, Nothing) -> die $ missingErr $ prettyShow' pkgId
    Just (package, _) -> return package
  where
    missingErr = format ("could not find package\
                         \: "%s%" in nix's all-cabal-hashes tar file")
    withVersions
      = (fmap pairWithVersion)
      . (fmap lineToText)
      . sed baseName
      $ grepPkg xs
    grepPkg = grep $ has $ Turtle.text search

    pairWithVersion z = (z, listToMaybe $ catMaybes $ match extractVersion z)
    extractVersion = do
      void $ "all-cabal-hashes-" >> (selfless $ plus $ noneOf "/")
      void $ "/"
      pname <- selfless $ plus $ noneOf "/"
      void $ "/"
      pversion <- selfless $ plus $ noneOf "/"
      void $ "/"
      void $ selfless $ star anyChar
      return $ parseVersion $ format (""%s%"-"%s%"") pname pversion

    baseName = do
      void $ selfless $ star anyChar
      start <- "all-cabal-hashes-"
      rest <- star anyChar
      pure $ start <> rest

    search = if pkgVersion == nullVersion
             then format ("/"%s%".cabal") name'
             else format ("/"%s%"/"%s%"/"%s%".cabal") name' version' name'
      where
        name' = prettyShow' pkgName
        version' = prettyShow' pkgVersion

{- | Decode JSON text into aeson's 'Aeson.Value'. Invalid JSON values are
    implicitly discarded.
>>> view $ json $ select ["{ \"ke", "y\": [", " 1] }[", "]1"]
Object (fromList [("key",Array [Number 1.0])])
Array []
Number 1.0
>>> view $ json $ select ["][]"]
Array []
-}
jsonValue :: Shell Text -> Shell Aeson.Value
jsonValue someText = Shell _foldIO'
  where
    _foldIO' (FoldShell step begin done) = foldIO someText
        (Foldl.premapM (return . Text.encodeUtf8) (FoldM step' begin' done'))
      where
        step' (z, r) bs = case A.feed r bs of
          A.Fail leftover _ _ ->
            step' (z, r0) (ByteString.drop 1 leftover)
          r'@(A.Partial {}) -> return (z, r')
          r'@(A.Done leftover val) -> do
            z' <- step z val
            if ByteString.null leftover
              then return (z', r')
              else step' (z', r0) leftover
        begin' = return (begin, r0)
        done' (z, r) = case r of
          A.Partial {} -> do
            (z', _) <- step' (z, r) ""
            done z'
          _ -> done z
        r0 = A.Partial (A.parse Aeson.value)

-- | Parses data directly from a 'Shell' 'Text'.
--
-- It dies if the data cannot be parsed.
json :: Aeson.FromJSON a => Shell Text -> IO a
json someText = singleJson someText >>= checkResult
  where
    singleJson = single . (fmap Aeson.fromJSON) . jsonValue
    checkResult = \case
      (Aeson.Success a) -> return a
      (Aeson.Error badParse) -> do
        let msg = format ("json': parse failed: "%s%"") errText
            errText = Text.pack badParse
        die msg

-- | Copy all the relative paths specified by targets under root to the
-- destination.
cpTo :: MonadIO io => FilePath -> FilePath -> Shell FilePath -> io ()
cpTo dst dstRoot targets = sh $ do
  t <- targets
  let newt = fromText $ Text.replace "nix/" "" $ format fp t
      newPath = dst </> newt
      oldPath = dstRoot </> t
  mktree (directory newPath)
  cp oldPath newPath
  liftIO $ printf ("copied "%fp%" to "%fp%"\n") oldPath newPath

-- | Creates a 'Fold' which searches some 'Text' for a nix field with the given
-- name.
findNixField :: Text -> Fold Text ((Maybe Text), [Text])
findNixField name = (,) <$> findNixField' <*> Foldl.list
  where
    findNixField' = Fold foldFunc Nothing id
    foldFunc = mkStepFunc $ nixField name
    mkStepFunc origF = f'
      where f' Nothing z = origF z
            f' y _       = y
    nixField nm nms =
      let findValue = do
            void $ spaces1
            void $ Turtle.text nm
            void " = \""
            value <- selfless (plus (noneOf "\""))
            void $ "\""
            return value
      in listToMaybe $ match (prefix findValue) nms

-- | Attempts to parse the input as a cabal uri: cabal://<pkg-id>.
parsePkgId :: Text -> Maybe PackageIdentifier
parsePkgId uri =
  let findPkgId = do
        void $ "cabal://"
        pkgId <- (star anyChar)
        return $ simpleParse' pkgId
  in join $ listToMaybe $ match (findPkgId) uri

-- | A 'Pattern' that matches any file the could have been created by the
-- haskell-overridez tool.
allOverridezPat :: Pattern Text
allOverridezPat = Foldl.fold (Fold (<|>) empty id)
  ([ suffix ("nix/nix-expr/" <> notSlashes <> ".nix")
   , suffix ("nix/git-json/" <> notSlashes <> ".json")
   ] <> map (suffix . Turtle.text . optionsPath) knownCabalOpts)
  where
    optionsPath = ((<>) "nix/options/") . firstLower . Text.pack . show
    notSlashes = selfless (plus (noneOf "/"))

-- | The supported cabal option overrides.
data CabalOpt =
  DoJailbreak
  | DontCheck
  | DontHaddock
  deriving (Eq, Read, Show, Enum, Bounded)

-- | The supported cabal option overrides.
knownCabalOpts :: [CabalOpt]
knownCabalOpts = [minBound .. maxBound]

-- | Determine the path of the configuration file for a 'CabalOpt'.
configPath :: CabalOpt -> FilePath
configPath = fromText . ((<>) "nix/options/") . firstLower . Text.pack . show

-- | Lowercase the first character of some text.
firstLower :: Text -> Text
firstLower = uncurry (<>) . first Text.toLower . Text.splitAt 1

-- | Remove a line from a file if it is present.
removeLine :: Text -> FilePath -> IO ()
removeLine someText src = do
  exists <- testfile src
  when exists $ do
    initial <- fold (input src) Foldl.list
    let allow = (/=) someText  . lineToText
    output src $ mfilter allow $ select initial

-- | Add a line to a text file if it is not already present.
addLine :: MonadIO io => Line -> FilePath -> io ()
addLine aLine p = liftIO $ testfile p >>= \case
  False -> mktree (directory p) >> (output p $ return aLine)
  _ -> do
    initial <- fold (input p) Foldl.list
    output p $ uniq $ (select initial) <|> return aLine

shTextToLines :: Shell Text -> Shell Line
shTextToLines = join . fmap (select . toList . textToLines)

prettyShow' :: Pretty a => a -> Text
prettyShow' = Text.pack . prettyShow

simpleParse' :: DistText.Text a => Text -> Maybe a
simpleParse' = simpleParse . Text.unpack

parseVersion :: Text -> Maybe Version
parseVersion = (fmap pkgVersion) . simpleParse'

lookupEnv :: MonadIO io => Text -> io (Maybe Text)
lookupEnv name = env >>= (return . lookup name)

allHashesEnv :: Text
allHashesEnv = "HOZ_ALL_CABAL_HASHES"

-- | The path of the haskell-overridez nix-expr library loader.
loaderPath :: FilePath
loaderPath = "nix/haskell-overridez.nix"

-- | The path to save prefetch github json metadata
githubJsonPath :: FilePath
githubJsonPath = "github.json"

currentVersion, archiveUrlPrefix, archiveUrl, archiveUrlSuffix :: Text
archiveUrlPrefix = "https://github.com/adetokunbo/haskell-overridez/archive/v"
archiveUrlSuffix = ".tar.gz"
currentVersion = Text.pack $ showVersion version
archiveUrl = archiveUrlPrefix <> currentVersion <> archiveUrlSuffix

-- | The command used to compute the hash of the library loader git repository.
computeLoaderHashCmd :: Text
computeLoaderHashCmd = "nix-prefetch-url --unpack " <> archiveUrl

-- | Generate the content of the library loader nix-expr file.
computeLoaderContent :: Text -> Text -> Text
computeLoaderContent tarUrl h = [text|
let
  pkgs = import <nixpkgs> {};
  overridez = fetchTarball {
    url = "$tarUrl";
    sha256 = "$h";
  };
in
  import overridez { inherit pkgs; }
|]
