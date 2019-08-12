{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
  ( main
  ) where

import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Drive          as D
import qualified Drive.Browser  as B
import qualified Drive.Describe as D


ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
ff = D.foldFree


header :: String -> IO ()
header t = putStrLn ("\n\n# " <> t)


subheader :: String -> IO ()
subheader t = putStrLn ("\n-- " <> t <> "\n")


readPageTitle :: D.Free B.BrowserF Text
readPageTitle = do
  B.goToUrl (B.Url "https://www.google.com/")
  B.readTitle


main :: IO ()
main = do
  header "Browser"
  let p = readPageTitle

  subheader "converting to describe:"
  (ff D.execDescribe . ff B.browserToDescribeI) p >>= print

  subheader "running in browser:"
  B.runDefaultSession (ff B.execBrowser p) >>= print

  subheader "interleaving:"
  let i = D.sumI B.browserToDescribeI D.identityI
  let r = D.bimapI D.execDescribe B.execBrowser
  B.runDefaultSession ((ff r . ff i) p) >>= print
