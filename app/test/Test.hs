module Main
  ( main
  ) where

import qualified Test.Tasty          as Tasty

import qualified Test.Drive.Basics   as Basics01
import qualified Test.Drive.Basics03 as Basics03

main :: IO ()
main = Tasty.defaultMain $
  Tasty.testGroup "main"
    [ Basics01.tests
    , Basics03.tests
    ]
