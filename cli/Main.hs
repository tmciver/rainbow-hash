module Main where

import RainbowHash
import System.IO
import System.FilePath
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import qualified Data.ByteString as B
import Control.Monad (join)

-- |Asks the user for the directory from which files will be imported.
getSourceDir :: IO FilePath
getSourceDir = do
  currentDir <- getCurrentDirectory
  putStr ("Enter a source directory [" ++ currentDir ++ "]: ")
  hFlush stdout
  d <- getLine
  let tmpDir = if null d then currentDir else d
  dirExists <- doesDirectoryExist tmpDir
  let dir = if dirExists then tmpDir
            else error ("Error: " ++ tmpDir ++ " does not exist.")
  return dir

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively fp = do
  isDir <- doesDirectoryExist fp
  if isDir then do
    fs <- listDirectory fp
    let fsAbs = (fp </>) <$> fs
    ffs <- traverse listFilesRecursively fsAbs
    return $ join ffs
    else return [fp]

putFile' :: FilePath -> IO ()
putFile' fp = do
  h <- putFile fp
  putStrLn ("Stored file whose content hash is: " ++ h)

putFilesFromDirectory :: FilePath -> IO ()
putFilesFromDirectory fp = do
  fs <- listFilesRecursively fp
  _ <- traverse putFile' fs
  return ()

main :: IO ()
main = do
  sd <- getSourceDir
  putFilesFromDirectory sd
  putStrLn "Done."
