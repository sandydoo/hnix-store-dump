module Main where

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Nix.Nar

main :: IO ()
main = do
  [storePath] <- getArgs

  pwd <- getCurrentDirectory
  let narFilePath = pwd </> "nar.dump"

  -- Create a new file and write the nar to it
  withBinaryFile narFilePath WriteMode $ \h -> do
    buildNarIO narEffectsIO storePath h

    putStrLn $ "Wrote nar to " ++ narFilePath
