module Examples.Intercom.Interpreters
  ( verboseLogging
  , debugExecute
  , deepExecute
  ) where

import Control.Monad.Reader
import qualified Drive                as D
import qualified Drive.HTTP           as H
import qualified Drive.Describe            as D
import qualified Drive.Intercom       as I


type DoubleLog = D.DescribeF D.:+: D.DescribeF
type CredReq = H.HttpHeaderF I.IntercomCredentials
type LogCred = D.DescribeF D.:+: CredReq
type DoubleLogCred = DoubleLog D.:+: CredReq


verboseLogging
  :: ( D.Interpreter I.IntercomF DoubleLog a
     , DoubleLog a -> IO a
     )

verboseLogging = (i, r)
  where
    i = D.sumI
          I.intercomToDescribeI
          (D.composeI (H.httpHeaderToLog I.emptyCredentials) I.intercomToNetworkI)

    r = D.bimapI D.execDescribe D.execDescribe


debugExecute
  :: ( D.Interpreter I.IntercomF LogCred a
     , I.IntercomCredentials -> LogCred a -> IO a
     )

debugExecute = (i, r)
  where
    i = D.sumI
          I.intercomToDescribeI
          I.intercomToNetworkI

    r c = D.bimapI
          D.execDescribe
          (\x -> runReaderT (H.execHttpHeader x) c)


deepExecute
  :: ( D.Interpreter I.IntercomF DoubleLogCred a
     , I.IntercomCredentials -> DoubleLogCred a -> IO a
     )

deepExecute = (i, r)
  where
    i = D.sumI
          (D.sumI
            I.intercomToDescribeI
            (D.composeI (H.httpHeaderToLog I.emptyCredentials) I.intercomToNetworkI)
          )
          I.intercomToNetworkI

    r c = D.bimapI
          (D.bimapI
            D.execDescribe
            D.execDescribe
          )
          (\x -> runReaderT (H.execHttpHeader x) c)
