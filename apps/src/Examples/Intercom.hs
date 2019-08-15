module Main
  ( main
  ) where

import           Control.Monad.Reader
import qualified Data.Yaml                      as Y
import           Examples.Intercom.Interpreters
import           Examples.Intercom.Programs


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
  a <- Y.decodeFileEither "./credentials/intercom.yaml"

  case a of
    Left _ -> pure (Left NoCredsError)
    Right c  -> do
      r <- runReaderT (runProg p) c
      pure (Right r)

  where
    runProg p' = do
      runDescribeR (asVerbose p')
      runHttp (asHttp p')
