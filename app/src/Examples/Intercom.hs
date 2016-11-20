module Main
  ( main
  ) where

import qualified Data.Yaml            as Y
import qualified Drive                as D
import qualified Drive.Intercom       as I
import Examples.Intercom.Programs
import Examples.Intercom.Interpreters

data IntercomError
  = NoCredsError
  | Bored
  deriving (Show, Eq)


type IntercomP a = D.Free I.IntercomF a


ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
ff = D.foldFree


main :: IO ()
main = runIntercomProgram p >>= print
  where
    p = fetchAllAdmins


runIntercomProgram
  :: IntercomP a
  -> IO (Either IntercomError a)

runIntercomProgram p = do
  a <- Y.decodeFile "./credentials/intercom.yaml"

  case a of
    Nothing -> pure (Left NoCredsError)
    Just c  -> Right <$> runIntercom c p

  where
    -- (i, r) = verboseLogging
    (i, r) = debugExecute

    runIntercom :: I.IntercomCredentials -> IntercomP a -> IO a
    runIntercom c = ff (r c) . ff i

