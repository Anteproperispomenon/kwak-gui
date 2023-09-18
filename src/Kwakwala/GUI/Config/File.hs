module Kwakwala.GUI.Config.File
  ( findAndCreateConf
  , updateConfigFile
  , updateConfigFile'
  ) where

import Control.Exception

import Data.Bifunctor (first, second)
import Data.Ini.Config.Bidir
import Data.Default
import Data.Text qualified as T
import Data.Text (Text)

import Kwakwala.GUI.Exception
import Kwakwala.GUI.Config
import Kwakwala.GUI.Config.Parsing

import System.Directory
import System.FilePath

import TextUTF8 qualified as TU

-- | Try and read/create the config file
-- in AppData/AppConfig/etc...
findAndCreateConf :: IO (FilePath, (Either Text (Ini KwakConfigModel)))
findAndCreateConf = do
  eCfgDir <- try @SomeException $ getXdgDirectory XdgConfig ""
  case eCfgDir of
    -- Can't obtain AppData/etc...
    Left _ -> do
      dir <- getCurrentDirectory
      createConf dir
    -- Could Find AppData
    Right dir -> do
      let kwkDir = dir </> "kwak-gui"
      eRslt <- try @SomeException $ createDirectoryIfMissing True kwkDir
      case eRslt of
        -- Can't make directory in AppData
        Left _ -> do
          cdir <- getCurrentDirectory
          createConf cdir
        -- Can make directory in AppData.
        Right _ -> createConf kwkDir

createConf :: FilePath -> IO (FilePath, (Either Text (Ini KwakConfigModel)))
createConf dir = do
  let kwkFile = dir </> "kwak-config.ini"
  bl <- doesFileExist kwkFile
  case bl of
    False -> do
      let defIni = ini def configSpec
          iniTxt = serializeIni defIni
      TU.writeFile kwkFile iniTxt -- maybe encase in Try?
      return (kwkFile, Right defIni)
    True  -> do
      -- okay
      eTxt <- try @SomeException $ TU.readFile kwkFile
      case eTxt of
        Left e -> do
          return (kwkFile, Left $ T.pack $ show e)
        Right txt -> return (kwkFile, parseConfig txt)

-- | Update the config file by re-parsing it
-- and updating the resultant value.
updateConfigFile' :: FilePath -> KwakConfigModel -> IO (Maybe Text)
updateConfigFile' fp kcm = ignoreResult <$> do
  tryCont' @SomeException (TU.readFile fp) $ \txt -> 
    tryContP (first T.pack $ parseIni txt (ini kcm configSpec)) $ \tmpIni ->
        let newIni = setUpdateAddOptional tmpIni
            nwrIni = updateIni kcm newIni
            outIni = serializeIni nwrIni
        in tryContF @SomeException $ TU.writeFile fp outIni
-- NOTE: Instead of opening and re-opening the file
-- separately, maybe open the file once in ReadWriteMode?

{-
  case eTxt of
    Left  e   -> return $ Just T.pack $ displayException e
    Right txt -> do
      let eIni = parseIni
-}

-- | Update the config file using the current
-- value of the configuration.
updateConfigFile :: FilePath -> Ini KwakConfigModel -> IO ()
updateConfigFile fp _ = return ()
  -- hmm...

-- Alternative Proposal:
-- only store a KwakConfigModel
-- in the app model, and re-parse
-- the config file each time.
-- Then, run `updateIni` on the
-- result with the new config value.

