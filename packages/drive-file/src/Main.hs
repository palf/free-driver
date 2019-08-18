{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Drive          as V

import           Data.Monoid    ((<>))
import           Drive          ((>--->))
import           Drive.Describe
import           Drive.File


program :: FileP ()
program
  = write "some content"


main :: IO ()
main = do
  describe program
  run program

  where
    describe
      = fileToDescribeI >---> execDescribe

    run
      = withFile "write-example.txt"
      . (V.identityI >---> execFile)


fileToDescribeI :: FileF a -> DescribeP a
fileToDescribeI (WriteFile t a) = a <$ debug ("writing to file \"" <> t <> "\"")
