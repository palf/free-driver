module Main
  ( main
  ) where

import qualified Data.Text     as Tx
import qualified Drive         as D
import qualified Drive.Browser as B
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


readPageTitle :: D.Free B.BrowserF Tx.Text
readPageTitle = do
  B.goToUrl (B.Url "https://www.fish.com/")
  B.readTitle


-- causeNoSuchElementError :: D.Free B.BrowserF Text
-- causeNoSuchElementError = do
--   goToUrl (Url "http://localhost:3000")
--   x <- readText (CSS "li:nth-child(4)")
--   pure x


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
