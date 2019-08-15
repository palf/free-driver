{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Drive          as V
import           Drive.Describe


program :: DescribeP ()
program = do
  verbose "some verbose data"
  debug "some debug data"


main :: IO ()
main = V.foldFree execDescribe program
