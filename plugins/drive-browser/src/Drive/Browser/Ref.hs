module Drive.Browser.Ref
  ( Ref (..)
  , showRef
  ) where

import qualified Data.Text as T


data Ref
  = CSS T.Text
  | LinkText T.Text


showRef :: Ref -> T.Text
showRef (CSS r)      = r
showRef (LinkText r) = r
