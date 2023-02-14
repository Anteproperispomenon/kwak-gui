module Kwakwala.GUI.Config.File
  ( findAndCreateConf

  ) where

import Control.Exception

import Data.Ini.Config.Bidir
import Data.Default
import Data.Text qualified as T
import Data.Text (Text)

import Kwakwala.GUI.Config
import Kwakwala.GUI.Config.Parsing

import System.Directory
import System.FilePath

import TextUTF8 qualified as TU

-- | Try and read/create the 
findAndCreateConf :: IO (Either Text (Ini KwakConfigModel))
findAndCreateConf = do
  eCfgDir <- try @SomeException $ getXdgDirectory XdgConfig ""
  case eCfgDir of
    Left _ -> do
      dir <- getCurrentDirectory
      createConf dir
    Right dir -> do
      let kwkDir = dir </> "kwak-gui"
      eRslt <- try @SomeException $ createDirectoryIfMissing True kwkDir
      case eRslt of
        Left _ -> do
          cdir <- getCurrentDirectory
          createConf cdir
        Right _ -> createConf kwkDir

createConf :: FilePath -> IO (Either Text (Ini KwakConfigModel))
createConf dir = do
  let kwkFile = dir </> "kwak-config.ini"
  bl <- doesFileExist kwkFile
  case bl of
    False -> do
      let defIni = ini def configSpec
          iniTxt = serializeIni defIni
      TU.writeFile kwkFile iniTxt
      return $ Right defIni -- TEMP
    True  -> do
      -- okay
      eTxt <- try @SomeException $ TU.readFile kwkFile
      case eTxt of
        Left e -> do
          -- hmm...
          return $ Left $ T.pack $ show e
        Right txt -> return $ parseConfig txt            
          

      