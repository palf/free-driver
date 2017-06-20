module Main
  ( main
  ) where

import qualified Data.Yaml            as Y
import Control.Monad.Reader
import Examples.Intercom.Programs
import Examples.Intercom.Interpreters


data AppError
  = NoCredsError
  deriving (Show, Eq)


main :: IO ()
main = runIntercomProgram p >>= print
  where
    p = fetchAllAdmins


runIntercomProgram
  :: IntercomP a
  -> IO
       ( Either AppError
         ( Either HError
           ( Either IError a )
         )
       )

runIntercomProgram p = do
  a <- Y.decodeFile "./credentials/intercom.yaml"

  case a of
    Nothing -> pure (Left NoCredsError)
    Just c  -> do
      r <- runReaderT (runProg p) c
      pure (Right r)

  where
    runProg p' = do
      runDescribeR (asVerbose p')
      runHttp (asHttp p')
