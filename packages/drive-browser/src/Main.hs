{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Main
  ( main
  ) where

import           Data.Text      (Text)
import           Drive
import qualified Drive          as V
import           Drive.Browser
import           Drive.Describe


program :: BrowserP Text
program = do
  goToUrl (Url "http://www.google.com")
  readTitle


main :: IO ()
main = do
  describe program >>= print
  runWebdriver program >>= print
  runAndDescribe program >>= print

  where
    describe
      = browserToDescribeI >---> execDescribe

    runWebdriver
      = runDefaultSession . (V.identityI >---> execBrowser)

    runAndDescribe p = do
      let i = browserToDescribeI <---> identityI
      let r = execDescribe >---< execBrowser
      runDefaultSession ((i >---> r) p)

