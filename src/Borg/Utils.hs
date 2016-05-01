-- | Utilities.

module Borg.Utils
  (
  -- * Utilities
    anyM
  , notifyOSX
  ) where

import qualified Data.Text as T (Text)
import Data.Monoid ((<>))
import Shelly.Lifted (MonadSh, run_)

-- | A version of 'any' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > anyM Just [False,True ,undefined] == Just True
-- > anyM Just [False,False,undefined] == undefined
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x:xs) = do
  t <- p x
  if t then pure True else anyM p xs

notifyOSX :: (MonadSh m)
          => T.Text -> T.Text -> m ()
notifyOSX title msg =
  run_ "osascript" $ [ "-e"
    , "display notification \"" <> msg <> "\" with title \"" <> title <> "\""
    ]
