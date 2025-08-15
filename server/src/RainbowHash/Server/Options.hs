{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.Server.Options
  ( getOptionsParser
  , Options(..)
  ) where

import Protolude
import Numeric.Natural (Natural)
import System.Directory (getXdgDirectory, XdgDirectory (XdgData))

import Options.Applicative (Parser, metavar, strOption, value, option, auto, long, short, help, helper, info, progDesc, ParserInfo, fullDesc, header)

data Options = Options
  { storageDir :: FilePath
  , port :: Natural
  }

defaultPort :: Natural
defaultPort = 3000

storageDirParser
  :: FilePath
  -> Parser FilePath
storageDirParser defStorageDir = strOption
  ( long "storage-dir"
    <> help ("The directory where the server will store file data. Default: " <> defStorageDir)
    <> short 'd'
    <> metavar "DIRECTORY"
    <> value defStorageDir
  )

portParser :: Parser Natural
portParser = option auto
  ( long "port"
    <> help ("The port the application runs on. Default: " <> show defaultPort)
    <> short 'p'
    <> metavar "PORT"
    <> value defaultPort
  )

optionsParser
  :: FilePath
  -> Parser Options
optionsParser defStorageDir = Options <$> storageDirParser defStorageDir <*> portParser

optionsParserInfo
  :: FilePath
  -> ParserInfo Options
optionsParserInfo storageDirectory =
  info (optionsParser storageDirectory <**> helper)
    ( fullDesc
      <> progDesc "A web server for handling content-addressable file storage."
      <> header "Runs the rainbow-hash web server."
    )

getDefaultStorageDir :: IO FilePath
getDefaultStorageDir = getXdgDirectory XdgData "rainbowhash"

getOptionsParser :: IO (ParserInfo Options)
getOptionsParser = getDefaultStorageDir <&> optionsParserInfo
