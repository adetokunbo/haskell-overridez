{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Control.Monad.Log
import           Data.Text.Prettyprint.Doc (Doc)
import           System.IO (stdout)

main :: IO ()
main =
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \logToStdout ->
  runLoggingT testApp (logToStdout . renderWithSeverity id)


testApp :: MonadLog (WithSeverity (Doc ann)) m => m ()
testApp = do
  logMessage (WithSeverity Informational "Don't mind me")
  logMessage (WithSeverity Error "But do mind me!")
