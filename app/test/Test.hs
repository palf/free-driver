module Main
  ( main
  ) where

import qualified Test.Drive.Basics03 as Basics03
import qualified Test.Tasty          as Tasty

main :: IO ()
main = Tasty.defaultMain $
  Tasty.testGroup "main"
    [ Basics03.tests
    ]
