{-# LANGUAGE ExplicitForAll #-}

module Main
  ( main
  ) where

import           Data.Monoid    ((<>))
import qualified Drive          as D
import qualified Drive.Describe as D
import qualified Drive.File     as F


ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
ff = D.foldFree


header :: String -> IO ()
header t = putStrLn ("\n\n# " <> t)


subheader :: String -> IO ()
subheader t = putStrLn ("\n-- " <> t <> "\n")


exampleWriteToFile :: D.Free F.FileF ()
exampleWriteToFile
  = F.write "some content"


main :: IO ()
main = do
  header "File"
  let p = exampleWriteToFile

  subheader "file operations to describe"
  (ff D.execDescribe . ff fileToLog) p

  subheader "file operations to file"
  F.withFile "example.log" (ff F.execFile p)


fileToLog :: D.Interpreter F.FileF D.DescribeF a
fileToLog (F.WriteFile t a) = a <$ D.debug ("writing to file \"" <> t <> "\"")


-- logToFile :: D.Interpreter D.DescribeF F.FileF a
-- logToFile (D.LogEntry _ t a) = a <$ F.write t
