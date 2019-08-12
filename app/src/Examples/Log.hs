module Main
  ( main
  ) where

import qualified Drive          as D
import qualified Drive.Describe as D


main :: IO ()
main = D.foldFree D.execDescribe p

  where
    p = do
      D.verbose "some verbose data"
      D.debug "some debug data"
