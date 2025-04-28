{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RainbowHash.Logger (writeLog) where

import Protolude

import Control.Monad.Logger (LogLevel(..))
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)
import System.Environment (lookupEnv)

getLogLevelConfig :: IO LogLevel
getLogLevelConfig = lookupEnv "LOG_LEVEL" <&> fromMaybe LevelInfo . (>>= readLogLevel . T.pack)

showLogLevel :: LogLevel -> Text
showLogLevel (LevelOther lvl) = lvl
showLogLevel lvl = T.drop 5 . show $ lvl

readLogLevel :: Text -> Maybe LogLevel
readLogLevel levelText = case T.toLower levelText of
  "debug" -> Just LevelDebug
  "info" -> Just LevelInfo
  "warn" -> Just LevelWarn
  "error" -> Just LevelError
  _ -> Nothing

writeLog :: LogLevel -> Text -> IO ()
writeLog level msg = do
  configLevel <- getLogLevelConfig
  when (level >= configLevel) $ do
    time <- getCurrentTime
    putStrLn $ showTime time <> " " <> showLogLevel level <> " " <> msg
  where showTime :: UTCTime -> Text
        showTime = T.pack . formatTime defaultTimeLocale "%F %T%Z"
