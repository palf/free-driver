module Drive.Browser.Ref
  ( Ref (..)
  , showRef
  ) where

import           Data.Text (Text)


data Ref
  = CSS Text
  | LinkText Text


showRef :: Ref -> Text
showRef (CSS r)      = r
showRef (LinkText r) = r
