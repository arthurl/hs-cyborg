-- | A module to query the connection mode (i.e. metered / unmetered).

module Borg.Connection
  (
  -- * Connection mode
    isUnmeteredConn
  ) where

import Borg.Utils

import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Safe (headMay, atMay)
import Shelly.Lifted (Sh, MonadSh(..), run, silently)

-- | Returns a list of tuples (hardware port, device). Hardware port is in lower
-- case.
getDeviceHardwarePorts :: Sh [(T.Text, T.Text)]
getDeviceHardwarePorts = do
  hwConfigRaw <- silently $ run "networksetup" ["-listallhardwareports"]
  let hwConfigs :: [[T.Text]]
      hwConfigs = map T.lines
                  . mapMaybe (T.stripPrefix "Hardware Port: ")
                  . T.splitOn "\n\n" . T.strip -- Separate entries by device
                  $ hwConfigRaw
  pure $ mapMaybe parseDeviceName hwConfigs
  where
    parseDeviceName :: [T.Text] -> Maybe (T.Text, T.Text)
    parseDeviceName hwConfig = do
      portName <- headMay hwConfig
      deviceName <- T.stripPrefix "Device: " =<< hwConfig `atMay` 1
      pure (T.toLower portName, deviceName)

-- | Given a device name (e.g. en0), query the system to see if it is active.
isDeviceActive :: (Foldable f) => f T.Text -> T.Text -> Sh Bool
isDeviceActive activeKw device = do
  ifCfgOut <- silently $ run "ifconfig" [device]
  pure $ any (`T.isInfixOf` ifCfgOut) activeKw

-- | Checks if the system is connected to network via an unmetered connection.
isUnmeteredConn :: (Foldable f1, Foldable f2, MonadSh m) =>
                   f1 T.Text -> f2 T.Text -> m Bool
isUnmeteredConn activeKw unmeteredConnNames = do
  hwPortDeviceDict <- liftSh getDeviceHardwarePorts
  let unmeteredDevices =
        concatMap (findDevicesFromPort hwPortDeviceDict) unmeteredConnNames
  liftSh $ anyM (isDeviceActive activeKw) unmeteredDevices
  where
    findDevicesFromPort :: [(T.Text, a)] -> T.Text -> [a]
    findDevicesFromPort dict port =
      [ device' | (port',device') <- dict, port `T.isInfixOf` port' ]
