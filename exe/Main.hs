{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import System.IO
import System.Nix.Nar

newtype Options = Options Command

data Command
  = DumpNar DumpNarOptions
  | RestoreNar RestoreNarOptions

data DumpNarOptions = DumpNarOptions
  { dumpStorePath :: FilePath,
    dumpOutPath :: FilePath
  }

data RestoreNarOptions = RestoreNarOptions
  { restoreNarPath :: FilePath,
    restoreOutPath :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command "dump" (info (DumpNar <$> dumpNarOptionsParser) (progDesc "Dump the NAR for a store path")),
        command "restore" (info (RestoreNar <$> restoreNarOptionsParser) (progDesc "Restore a NAR to a directory"))
      ]

dumpNarOptionsParser :: Parser DumpNarOptions
dumpNarOptionsParser = do
  dumpStorePath <- argument str (metavar "STOREPATH")
  dumpOutPath <- option str (long "out" <> short 'o' <> metavar "OUTPATH")
  pure $ DumpNarOptions {..}

restoreNarOptionsParser :: Parser RestoreNarOptions
restoreNarOptionsParser = do
  restoreNarPath <- argument str (metavar "NARPATH")
  restoreOutPath <- option str (long "out" <> short 'o' <> metavar "OUTPATH")
  pure $ RestoreNarOptions {..}

main :: IO ()
main = do
  Options cmd <- execParser $ info (optionsParser <**> helper) fullDesc

  case cmd of
    DumpNar opts -> dumpNar opts
    RestoreNar opts -> restoreNar opts

dumpNar :: DumpNarOptions -> IO ()
dumpNar DumpNarOptions {..} = do
  -- Create a new file and write the nar to it
  withBinaryFile dumpOutPath WriteMode $ \h -> do
    buildNarIO narEffectsIO dumpStorePath h

    putStrLn $ "Wrote nar to " ++ dumpOutPath

restoreNar :: RestoreNarOptions -> IO ()
restoreNar RestoreNarOptions {..} = do
  res <- withBinaryFile restoreNarPath ReadMode $ \h ->
    runParser
      narEffectsIO
      parseNar
      h
      restoreOutPath

  print res
