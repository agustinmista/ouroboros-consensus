{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBAnalyser.HasAnalysis (Args, mkProtocolInfo)
import Codec.Serialise
import Control.Monad.Except
import DBAnalyser.Parsers
import Data.Bifunctor
import Main.Utf8
import Options.Applicative
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.StreamingLedgerTables
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as V1
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.StreamingLedgerTables
import qualified System.Directory as D
import System.FS.API
import System.FS.CRC
import System.FS.IO
import System.FilePath (splitDirectories)
import qualified System.FilePath as F

data Format
  = Mem FilePath
  | LMDB FilePath
  | LSM FilePath FilePath
  deriving (Show, Read)

data Config = Config
  { from :: Format
  -- ^ Which format the input snapshot is in
  , to :: Format
  -- ^ Which format the output snapshot must be in
  }

getCommandLineConfig :: IO (Config, CardanoBlockArgs)
getCommandLineConfig =
  execParser $
    info
      ((,) <$> (Config <$> parseConfig In <*> parseConfig Out) <*> parseCardanoArgs <**> helper)
      (fullDesc <> progDesc "Utility for converting snapshots to and from UTxO-HD")

data InOut = In | Out

inoutForGroup :: InOut -> String
inoutForGroup In = "Input arguments:"
inoutForGroup Out = "Output arguments:"

inoutForHelp :: InOut -> String -> String
inoutForHelp In = ("Input " ++)
inoutForHelp Out = ("Output " ++)

inoutForCommand :: InOut -> String -> String
inoutForCommand In = (++ "-in")
inoutForCommand Out = (++ "-out")

parseConfig :: InOut -> Parser Format
parseConfig io =
  ( Mem
      <$> parserOptionGroup
        (inoutForGroup io)
        (parsePath (inoutForCommand io "mem") (inoutForHelp io "snapshot dir"))
  )
    <|> ( LMDB
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lmdb") (inoutForHelp io "snapshot dir"))
        )
    <|> ( LSM
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lsm-snapshot") (inoutForHelp io "snapshot dir"))
            <*> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lsm-database") (inoutForHelp io "LSM database"))
        )

parsePath :: String -> String -> Parser FilePath
parsePath optName strHelp =
  strOption
    ( mconcat
        [ long optName
        , help strHelp
        , metavar "PATH"
        ]
    )

-- Helpers

-- | Given a filepath pointing to a snapshot (with or without a trailing slash), produce:
--
-- * A HasFS at the snapshot directory
pathToHasFS :: FilePath -> SomeHasFS IO
pathToHasFS (maybeRemoveTrailingSlash -> path) =
  SomeHasFS $ ioHasFS $ MountPoint path

maybeRemoveTrailingSlash :: String -> String
maybeRemoveTrailingSlash s = case last s of
  '/' -> init s
  '\\' -> init s
  _ -> s

defaultLMDBLimits :: V1.LMDBLimits
defaultLMDBLimits =
  V1.LMDBLimits
    { V1.lmdbMapSize = 16 * 1024 * 1024 * 1024
    , V1.lmdbMaxDatabases = 10
    , V1.lmdbMaxReaders = 16
    }

data Error blk
  = SnapshotError (SnapshotFailure blk)
  | ReadSnapshotCRCError FsPath CRCError
  deriving Exception

instance StandardHash blk => Show (Error blk) where
  show (SnapshotError err) =
    "Couldn't deserialize the snapshot. Are you running the same node version that created the snapshot? "
      <> show err
  show (ReadSnapshotCRCError fp err) = "An error occurred while reading the snapshot checksum at " <> show fp <> ": \n\t" <> show err

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  uncurry run =<< getCommandLineConfig
 where
  run :: Config -> Args (CardanoBlock StandardCrypto) -> IO ()
  run conf args = do
    ccfg <- configCodec . pInfoConfig <$> mkProtocolInfo args
    let
      getState :: SomeHasFS IO -> FsPath -> IO (LedgerState (CardanoBlock StandardCrypto) EmptyMK, CRC)
      getState fs path = do
        either
          (throwIO . SnapshotError . InitFailureRead @(CardanoBlock StandardCrypto) . ReadSnapshotFailed)
          (pure . first ledgerState)
          =<< runExceptT (readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode path)

    (st, fpInDir, f) <- case from conf of
      Mem fp@(pathToHasFS -> fs) -> do
        (st, _) <- getState fs (mkFsPath ["state"])
        pure (st, fp, fromInMemory (fp F.</> "tables" F.</> "tvar"))
      LMDB fp@(pathToHasFS -> fs) -> do
        (st, _) <- getState fs (mkFsPath ["state"])
        pure (st, fp, fromLMDB (fp F.</> "tables") defaultLMDBLimits)
      LSM fp@(pathToHasFS -> fs) lsmDbPath -> do
        (st, _) <- getState fs (mkFsPath ["state"])
        pure (st, fp, fromLSM lsmDbPath (last $ splitDirectories fp))

    let (fpOutDir, t) = case to conf of
          Mem fp -> (fp, toInMemory fp)
          LMDB fp -> (fp, toLMDB fp defaultLMDBLimits)
          LSM fp lsmDbPath -> (fp, toLSM lsmDbPath (last $ splitDirectories fp))

    D.createDirectoryIfMissing True fpOutDir

    D.copyFile (fpInDir F.</> "state") (fpOutDir F.</> "state")

    either throwIO pure =<< runExceptT (stream st f t)
