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

type Combo = TA TrelloF TerminalF
type ComboP a = Free Combo a
type Fuck a = Either HttpError (Either TrelloError a)



program :: ComboP [Text]
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
    describe = foldFree execDescribe . foldFree comboAsDescribe

    comboAsDescribe :: Combo a -> Free DescribeF a
    comboAsDescribe = bimapI trelloToDescribeI terminalToDescribeI

    runApi
      = foldEitherEitherFree (bimapI' execTrello execTerminal')

    execTerminal' :: (MonadIO m) => TerminalF a -> m (Fuck a)
    execTerminal' = fmap (Right . Right)  . execTerminal




bimapI'
  :: (Functor m)
  => (f a -> m (Fuck a))
  -> (g a -> m (Fuck a))
  -> ( f :+: g ) a
  -> m (Fuck a)

bimapI' f _ (D t) = f t
bimapI' _ f (R t) = f t


execTrello :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloF a -> m (Fuck a)
execTrello
  = foldEitherFree execHttpUri
  . trelloToNetworkI

