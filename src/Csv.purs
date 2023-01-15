module Csv where

import Prelude

import Data.Maybe (Maybe, fromMaybe)

optional :: forall a. Show a => Maybe a -> String
optional m = fromMaybe "" (show <$> m)
