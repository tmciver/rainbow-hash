{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO (hFlush)
import System.FilePath
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import Control.Monad (join)
import qualified System.Directory as D
import qualified Data.ByteString as BS

import RainbowHash
import RainbowHash.Env (Env(..))
import RainbowHash.App (runAppIO)
import RainbowHash.CLI (Command(..), getCommand)

rainbowHashDir :: IO FilePath
rainbowHashDir = D.getXdgDirectory D.XdgData "rainbowhash"

-- |Asks the user for the directory to which data will be stored.
getStoreDir :: IO FilePath
getStoreDir = do
  defaultStoreDir <- rainbowHashDir
  putStr ("Enter a storage directory [" ++ defaultStoreDir ++ "]: ")
  hFlush stdout
  d <- T.unpack <$> getLine
  let tmpDir = if null d then defaultStoreDir else d
  dirExists <- doesDirectoryExist tmpDir
  let dir = if dirExists then tmpDir
            else panic ("Error: " <> T.pack tmpDir <> " does not exist.")
  return dir

-- |Asks the user for the directory from which files will be imported.
getSourceDir :: IO FilePath
getSourceDir = do
  currentDir <- getCurrentDirectory
  putStr ("Enter a source directory [" ++ currentDir ++ "]: ")
  hFlush stdout
  d <- T.unpack <$> getLine
  let tmpDir = if null d then currentDir else d
  dirExists <- doesDirectoryExist tmpDir
  let dir = if dirExists then tmpDir
            else panic ("Error: " <> T.pack tmpDir <> " does not exist.")
  return dir

listFilesRecursively
  :: FilePath
  -> IO [FilePath]
listFilesRecursively fp = do
  isDir <- doesDirectoryExist fp
  if isDir then do
    fs <- listDirectory fp
    let fsAbs = (fp </>) <$> fs
    ffs <- traverse listFilesRecursively fsAbs
    return $ join ffs
    else return [fp]

putFile'
  :: FilePath -- ^Path to storage directory.
  -> FilePath -- ^Path to the file to store.
  -> IO ()
putFile' storeDir fp = do
  let env = Env storeDir
  FileId hash <- runAppIO (putFile fp "foo.txt") env
  T.putStrLn ("Stored file whose content hash is: " <> hash)

putFilesFromDirectory
  :: FilePath -- ^Path to storage directory.
  -> FilePath -- ^Path to source directory.
  -> IO ()
putFilesFromDirectory storeDir sourceDir = do
  fs <- listFilesRecursively sourceDir
  traverse_ (putFile' storeDir) fs

main :: IO ()
main = do
  eitherCommand <- getCommand
  case eitherCommand of
    Left err -> putStrLn err
    Right cmd -> case cmd of
      WatchDir dir -> putStrLn ("You want to watch directory: " <> T.pack dir)
