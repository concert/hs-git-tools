module Git.Objects.Types where

import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (TimeZone, ZonedTime, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Text.Printf (printf)

import Git.Sha1 (Sha1)
import Git.Types (FileMode)
