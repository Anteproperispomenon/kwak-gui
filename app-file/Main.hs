{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Lens
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Ini.Config.Bidir
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Monomer
import Monomer.Common.BasicTypes
import TextShow

import Kwakwala.GUI.Config
import Kwakwala.GUI.Config.File
import Kwakwala.GUI.Config.Parsing
import Kwakwala.GUI.Info
import Kwakwala.GUI.Types

import Kwakwala.GUI.Hidden
import Kwakwala.GUI.Widgets.AltAlert
import Kwakwala.GUI.Widgets.SaveFile

import qualified Monomer.Lens as L

import Monomer.Widgets.Containers.Scroll

import Monomer.Widgets.Singles.OptionButton
import Monomer.Widgets.Singles.TextArea

import Graphics.UI.TinyFileDialogs (saveFileDialog, openFileDialog)

import System.Directory
import System.FilePath
import System.IO

import TextUTF8 qualified as TU

-- from kwak-orth
import Kwakwala.Sounds

data AppModel = AppModel 
  { _inputOrth  :: InputOrth
  , _outputOrth :: OutputOrth
  , _inputFile  :: Text
  , _outputFile :: Text
  , _inputText  :: Text
  , _outputText :: Text
  , _currentDir :: Text -- Current Working Directory
  , _overwriteConfVis :: Bool
  , _errorAlertVis :: Bool
  , _writeSuccessVis :: Bool
  , _openErrorVis :: Bool
  , _configVis :: Bool
  , _sfmVis    :: Bool
  , _errorMsg :: Text
  , _kwakConfig :: KwakConfigModel
  , _sfmHidden :: HiddenVal SaveFileModel
  , _cfgFilePath :: FilePath
  -- , _kwakConfig :: Ini KwakConfigModel
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSetInput Text
  | AppSetOutput Text
  | AppOpenFile -- triggers dialog
  | AppSaveFile -- triggers dialog
  | AppSaveFileMan -- triggers manual output
  | AppWriteFile -- Actually writes content to file
  | AppGotInput (Maybe Text)
  | AppRefresh -- Refresh Output
  | AppRefreshI -- Refresh Input, to change the font.
  | AppCurDir FilePath
  | AppNull -- Empty event
  | AppWriteSuccess
  | AppWriteExists
  | AppWriteError Text
  | AppOverWrite -- when the user agrees to overwrite a file.
  | AppClosePopups
  | AppDoneConfig
  | AppOpenConfig
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack 
    [ button "Config" AppOpenConfig
    , spacer
    , hstack
      [ label "Input " `styleBasic` [textFont "Monotype"]
      , spacer
      , tooltipK ttUmista   $ optionButton_ "U'mista" IUmista (inputOrth) [onClick AppRefreshI]
      , spacer
      , tooltipK ttNapa     $ optionButton_ "NAPA" INapa (inputOrth) [onClick AppRefreshI]
      , spacer
      , tooltipK ttGrubb    $ optionButton_ "Grubb" IGrubb (inputOrth) [onClick AppRefreshI]
      , spacer
      , tooltipK ttBoas     $ optionButton_ "Boas" IBoas (inputOrth) [onClick AppRefreshI]
      , spacer
      , tooltipK ttGeorgian $ optionButton_ "Georgian" IGeorgian (inputOrth) [onClick AppRefreshI]
      , spacer
      , tooltipK ttIsland   $ optionButton_ "Island" IIsland (inputOrth) [onClick AppRefreshI]
      ]
    , spacer
    , popup_ configVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $ 
        box $ vstack
          -- [ kwakConfigWidgetX (kwakConfig . iniValueL)
          [ kwakConfigWidgetX kwakConfig
          , button "Done" AppDoneConfig
          ] `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]
    , popup_ sfmVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $ 
        box (selectSaveFileH sfmHidden inputFile convertSFEvent)
          `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]
    , hstack
      [ label "Output" `styleBasic` [textFont "Monotype"]
      , spacer
      , tooltipK ttUmista   $ optionButton_ "U'mista" OUmista (outputOrth) [onClick AppRefresh]
      , spacer
      , tooltipK ttNapa     $ optionButton_ "NAPA" ONapa (outputOrth) [onClick AppRefresh]
      , spacer
      , tooltipK ttGrubb    $ optionButton_ "Grubb" OGrubb (outputOrth) [onClick AppRefresh]
      , spacer
      , tooltipK ttBoas'    $ optionButton_ "Boas" OBoas (outputOrth) [onClick AppRefresh]
      , spacer
      , tooltipK ttGeorgian $ optionButton_ "Georgian" OGeorgian (outputOrth) [onClick AppRefresh]
      , spacer
      , tooltipK ttIsland   $ optionButton_ "Island" OIsland (outputOrth) [onClick AppRefresh]
      , spacer
      , tooltipK ttIpa      $ optionButton_ "IPA" OIpa (outputOrth) [onClick AppRefresh]
      ]
    , spacer
    , hgrid_ [childSpacing_ 8]
      [ vstack 
        [ button "Select File" AppOpenFile
        , spacer
        , (textField_ inputFile [readOnly])
        , spacer
        , (textArea_ inputText [readOnly]) `styleBasic` [textFont $ selectFontI $ model ^. inputOrth]
        ]
      -- , spacer
      , vstack
        [ hstack 
           [ box_ [expandContent, sizeReqUpdater sizeReqX] 
               (button "Choose Destination" AppSaveFile)
           , spacer -- filler
           , button "(Manual)" AppSaveFileMan
           ]
        , spacer
        , (textField_ outputFile [readOnly])
        , spacer
        , (textArea_ outputText [readOnly]) `styleBasic` [textFont $ selectFontO $ model ^. outputOrth]
        ]
      ]
    , spacer
    , button "Save File" AppWriteFile
    , popup overwriteConfVis (confirmMsg "File already Exists. Overwrite?" AppOverWrite AppClosePopups)
    , popup errorAlertVis (altAlertMsg_ (model ^. errorMsg) AppClosePopups [titleCaption "Error"]) `styleBasic` [textFont "Monotype"]
    , popup writeSuccessVis (alertMsg "File Saved Successfully." AppClosePopups)
    , popup openErrorVis (alertMsg "Could not open requested file." AppClosePopups)
    ] `styleBasic` [padding 10]

sizeReqX :: (SizeReq, SizeReq) -> (SizeReq, SizeReq)
sizeReqX (szrW, szrH)
  = (szrW {_szrFlex = 5, _szrExtra = 5, _szrFactor = 0.005}, szrH)

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Task $ AppCurDir <$> getCurrentDirectory]
  AppNull -> []
  -- AppIncrease -> [Model (model & clickCount +~ 1)]
  (AppSetInput  fnm) -> [Model (model & inputFile  .~ fnm), Task $ AppGotInput <$> readFileMaybe (T.unpack fnm)]
  (AppSetOutput fnm) -> [Model (model & outputFile .~ fnm & sfmVis .~ False)]
  AppOpenFile -> [Task $ handleFile1 <$> openFileDialog "Open Input File" "" ["*.txt", "*.*"] "Text Files" False]
  AppSaveFile -> [Task $ handleFile2 <$> saveFileDialog "Select Output File" (model ^. inputFile) ["*.txt", "*.*"] "Text Files"]
  AppSaveFileMan -> [Model (model & sfmVis .~ True)]
  AppGotInput mtxt -> case mtxt of
     (Just txt) -> [Model (model & inputText .~ txt & outputText .~ getConversion txt)]
     Nothing    -> [Model (model & openErrorVis .~ True)]
  -- Technically not a task anymore; it's just a bit too complicated to fit here.
  AppWriteFile -> [writeFileTask model (T.unpack (model ^. inputFile)) (T.unpack (model ^. outputFile)) (model ^. outputText)]
  AppWriteSuccess -> [Model (model & writeSuccessVis .~ True)] -- Display a pop-up message, maybe?
  AppWriteExists -> [Model (model & overwriteConfVis .~ True)]
  (AppWriteError err) -> [Model (model & errorMsg .~ (renderError err) & errorAlertVis .~ True), tlogErrTask err]
  AppOverWrite -> [Task $ overWriteFileTask (T.unpack (model ^. outputFile)) (model ^. outputText), Model (model & overwriteConfVis .~ False)] 
  AppRefresh  -> [Model (model & outputText .~ getConversion (model ^. inputText))]
  AppRefreshI -> [Model (model & outputText .~ getConversion (model ^. inputText) & inputText %~ modText)]
  AppClosePopups -> [Model (model & overwriteConfVis .~ False & errorAlertVis .~ False & writeSuccessVis .~ False & openErrorVis .~ False & configVis .~ False & sfmVis .~ False)]
  (AppCurDir fp) -> [Model (model & currentDir .~ (T.pack fp))]
  AppDoneConfig -> [Event AppRefresh, Model (model & configVis .~ False), Task $ writeConfigTask (model ^. cfgFilePath) (model ^. kwakConfig)]
  -- AppDoneConfig -> let newCfg = selfUpdate (model ^. kwakConfig)
  --   in [Model (model & configVis .~ False & kwakConfig .~ newCfg)] -- Add task here to update config file.
  AppOpenConfig -> [Model (model & configVis .~ True )]
  where 
    handleFile1 :: Maybe [Text] -> AppEvent
    handleFile1 Nothing = AppNull
    handleFile1 (Just []) = AppNull
    handleFile1 (Just (f:_fs)) = AppSetInput f
    handleFile2 :: Maybe Text -> AppEvent
    handleFile2 Nothing = AppNull
    handleFile2 (Just fnm) = AppSetOutput fnm
    getConversion :: Text -> Text
    getConversion inpTxt = let 
      inpO = model ^. inputOrth
      outO = model ^. outputOrth
      kcm  = model ^. kwakConfig
      in decodeKwakwalaD kcm outO $ parseKwakwalaD kcm inpO inpTxt
    inpDir :: Text
    inpDir = case (model ^. inputFile) of
      ""  -> ""
      txt -> let
        pat' = T.unpack txt
        in T.pack $ takeDirectory pat'
    renderError :: Text -> Text
    renderError err = "Error Trying to Save File:\n " <> err
    -- Slightly modify text by adding/removing a
    -- space at the end. This is to trigger a render
    -- update, so that the widget will use the new
    -- font specified in its style.
    -- Note: might want to have two copies
    -- of the input text in the model, one that
    -- is left unmodified, and one that is 
    -- modified and displayed.
    modText :: Text -> Text
    modText txt = case (T.unsnoc txt) of
      Nothing -> " "
      Just (txt', ' ') -> txt'
      Just (_txt,  _ ) -> (snoc txt ' ')

writeFileTask :: AppModel -> FilePath -> FilePath -> Text -> AppEventResponse AppModel AppEvent
writeFileTask model inp fp txt
  | (inp == "") = Event (AppWriteError $ "No input file selected yet. Choose an input file first.")
  | (inp == fp) = Event (AppWriteError $ "Can't overwrite input file; choose a different name for output file.")
  | (fp == "" ) = Event (AppWriteError $ "No output file selected; click \"Choose Destination\" to select an output file.")
  | (fp == ".") = Event (AppWriteError $ "No output file selected; click \"Choose Destination\" to select an output file.")
  | otherwise = Task $ do
      bl <- doesFileExist fp
      if bl
        then return AppWriteExists
        else do 
          eEvt <- try @SomeException (TU.writeFile fp txt)
          case eEvt of
            Left x   -> return (AppWriteError $ T.pack (show x))
            Right () -> return AppWriteSuccess
  where
    renderError :: Text -> Text
    renderError err = "Error Trying to Save File:\n " <> err


overWriteFileTask :: FilePath -> Text -> IO AppEvent
overWriteFileTask fp txt = do
  eEvt <- try @SomeException (TU.writeFile fp txt)
  case eEvt of
    Left x   -> return (AppWriteError $ T.pack (show x))
    Right () -> return AppWriteSuccess

writeConfigTask :: FilePath -> KwakConfigModel -> IO AppEvent
writeConfigTask fp kcm = do
  rslt <- updateConfigFile' fp kcm
  case rslt of
    Just err -> return $ AppWriteError ("Error writing config file:\n" <> err)
    Nothing  -> return AppNull

-- KurintoSansAux-Rg.ttf

convertSFEvent :: SFEvent -> AppEvent
convertSFEvent SFCancel = AppClosePopups
convertSFEvent (SFFilePath fp) = AppSetOutput fp

selectFontI :: InputOrth -> Font
selectFontI IUmista   = "Umista"
selectFontI INapa     = "NAPA"
selectFontI IGrubb    = "Universal"
selectFontI IGeorgian = "Georgian"
selectFontI IBoas     = "Boas"
selectFontI IIsland   = "Island"

selectFontO :: OutputOrth -> Font
selectFontO OUmista   = "Umista"
selectFontO ONapa     = "NAPA"
selectFontO OGrubb    = "Universal"
selectFontO OGeorgian = "Georgian"
selectFontO OBoas     = "Boas"
selectFontO OIsland   = "Island"
selectFontO OIpa      = "IPA"  

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe fp = do
  mtxt <- T.decodeUtf8' <$> BS.readFile fp
  case mtxt of
    Right txt -> return (Just txt)
    Left _    -> return Nothing

-- For logging errors to the terminal.
tlogErr :: Text -> IO ()
tlogErr txt = TIO.hPutStrLn stderr ("Error: " <> txt)

tlogErrTask :: Text -> AppEventResponse AppModel AppEvent
tlogErrTask txt = Task (tlogErr txt >> return AppNull)

main :: IO ()
main = do
  -- Set IO encoding to UTF-8
  hSetEncoding stdin  utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Actual code
  (cfgFile, eConf) <- findAndCreateConf
  startApp (model' cfgFile eConf) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Kwak'wala Orthography Conversion (File)",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Monotype" "./assets/fonts/LiberationMono-Regular.ttf",
      appFontDef "Universal" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "Georgian" "./assets/fonts/NotoSansGeorgian-Regular.ttf",
      appFontDef "Umista" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "NAPA" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "Boas" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "IPA" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "Island" "./assets/fonts/island.ttf",
      appInitEvent AppInit
      ]
    -- model = AppModel IUmista OUmista "" "" "" "" "" False False False False False "" def cfgFile
    -- Not using Ini in Model version:
    model' cfgFile (Left txt) = AppModel IUmista OUmista "" "" "" "" "" False True False False False False txt def def cfgFile
    model' cfgFile (Right iniX) = AppModel IUmista OUmista "" "" "" "" "" False False False False False False "" (getIniValue iniX) def cfgFile
    -- Using Ini in Model version:
    -- defIni = ini def configSpec
    -- model' cfgFile (Left txt) = AppModel IUmista OUmista "" "" "" "" "" False True False False False txt defIni cfgFile
    -- model' cfgFile (Right iniX) = AppModel IUmista OUmista "" "" "" "" "" False False False False False "" iniX cfgFile
