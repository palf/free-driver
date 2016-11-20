module Main
  ( main
  ) where

import qualified Drive        as D
import qualified Drive.Describe     as D

import Data.Monoid ((<>))

-- import Drive.NetworkSession
-- import Drive.Intercom.Conversation


ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
ff = D.foldFree


header :: String -> IO ()
header t = putStrLn ("\n\n# " <> t)


subheader :: String -> IO ()
subheader t = putStrLn ("\n-- " <> t <> "\n")


main :: IO ()
main = do
  header "Describe"


  subheader "verbose"
  ff D.execDescribe p

  where

    p = do
      D.verbose "some verbose data"
      D.debug "some debug data"
