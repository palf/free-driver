{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Reader   as R
import qualified Data.Text              as Text
import qualified Data.Yaml              as Y

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)

import           Drive
import           Drive.Describe
import           Drive.HTTP
import           Drive.Terminal
import           Drive.Trello


type ProgramF = Combo TrelloF TerminalF
type ProgramP a = Free ProgramF a
type Errors a = Either HttpError (Either TrelloError a)


program :: ProgramP [Text]
program = do
  i <- Text.pack <$> liftR readInput
  liftL $ fmap boardName <$> getBoards (User i)


withTrelloCredentials :: FilePath -> (TrelloAuth -> IO a) -> IO ()
withTrelloCredentials p f = Y.decodeFileEither p >>= either print (void . f)


main :: IO ()
main = do
  describe program >>= print

  withTrelloCredentials "credentials/trello.yaml" $ \auth -> do
    R.runReaderT (runApi program) auth >>= print
    print ("done" :: String)

  where
    describe = programAsDescribe >---> execDescribe

    programAsDescribe :: ProgramF a -> DescribeP a
    programAsDescribe = trelloToDescribeI >---< terminalToDescribeI

    runApi
      = foldEitherEitherFree (bimapI' execTrello execTerminal')

    execTerminal' :: (MonadIO m) => TerminalF a -> m (Errors a)
    execTerminal' = fmap (Right . Right)  . execTerminal



bimapI'
  :: (Functor m)
  => (f a -> m (Errors a))
  -> (g a -> m (Errors a))
  -> ( f :+: g ) a
  -> m (Errors a)

bimapI' f _ (L t) = f t
bimapI' _ f (R t) = f t


execTrello :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloF a -> m (Errors a)
execTrello
  = foldEitherFree execHttpUri
  . trelloToNetworkI

