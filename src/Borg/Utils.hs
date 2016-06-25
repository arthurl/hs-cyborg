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

{-
retrySh :: (Monoid a) => Int -> Sh a -> Sh a
retrySh 0 _ = mempty
retrysh 1 ...
retrySh n cmdSh = do
  output <- errExit False cmdSh
  exitCode <- lastExitCode
  if exitCode == 0 then pure (pure output) else
    retrySh (n-1) cmdSh
-}

{-
-- | Type level privilege control.
newtype Sudo a = Sudo { sudo :: Sh a }
runSudo :: T.Text -> [T.Text] -> Sudo T.Text
runSudo cmd' args' = Sudo $ run "/usr/bin/sudo" (cmd':args')

run_ "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/\
         \Current/Resources/airport" ["-I"]
-}
