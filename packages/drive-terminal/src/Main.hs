{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Main
  ( main
  ) where

import           Drive          ((<--->), (>---<), (>--->))
import qualified Drive          as V
import           Drive.Describe
import           Drive.Terminal


program :: TerminalP String
program = do
  name <- ask "name?"
  food <- ask "food?"
  pure (name <> " likes " <> food)

  where
    ask q = printMessage q >> readInput


main :: IO ()
main = do
  describe program >>= print
  runTerminal program >>= print
  describeRun program >>= print

  where
    describe = t2d >---> d2io
    runTerminal = t2t >---> t2io
    describeRun = (t2d <---> t2t) >---> (d2io >---< t2io)

    t2d = terminalToDescribeI
    t2t = V.identityI
    d2io = execDescribe
    t2io = execTerminal
