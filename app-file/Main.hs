{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Monomer
import TextShow

import Kwakwala.GUI.Types

import qualified Monomer.Lens as L

import Monomer.Widgets.Containers.Scroll

import Monomer.Widgets.Singles.OptionButton
import Monomer.Widgets.Singles.TextArea

import Graphics.UI.TinyFileDialogs (saveFileDialog, openFileDialog)

import System.Directory
import System.FilePath

import TextUTF8 qualified as TU

-- from kwak-orth
import Kwakwala.Sounds

data AppModel = AppModel 
  -- { _clickCount :: Int
  { _inputOrth  :: InputOrth
  , _outputOrth :: OutputOrth
  , _inputFile :: Text
  , _outputFile :: Text
  , _inputText :: Text
  , _outputText :: Text
  , _currentDir :: Text -- Current Working Directory
  , _overwriteConfVis :: Bool
  , _errorAlertVis :: Bool
  , _writeSuccessVis :: Bool
  , _errorMsg :: Text
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  -- | AppIncrease
  | AppSetInput Text
  | AppSetOutput Text
  | AppOpenFile -- triggers dialog
  | AppSaveFile -- triggers dialog
  | AppWriteFile -- Actually writes content to file
  | AppGotInput Text
  | AppRefresh -- Refresh Output
  | AppRefreshI -- Refresh Input, to change the font.
  | AppCurDir FilePath
  | AppNull -- Empty event
  | AppWriteSuccess
  | AppWriteExists
  | AppWriteError Text
  | AppOverWrite -- when the user agrees to overwrite a file.
  | AppClosePopups
  deriving (Eq, Show)

makeLenses 'AppModel

{-

label "Hello world"
    , spacer
    , hstack 
      [ label $ "Click count: " <> showt (model ^. clickCount)
      , spacer
      , button "Increase count" AppIncrease
      , spacer
      , label $ "Input: " <> showt (model ^. inputOrth)
      , spacer
      , label $ "Output: " <> showt (model ^. outputOrth)
      ]

-}

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack 
    [ hstack
      [ label "Input " `styleBasic` [textFont "Monotype"]
      , spacer
      , optionButton_ "U'mista" IUmista (inputOrth) [onClick AppRefreshI]
      , spacer
      , optionButton_ "NAPA" INapa (inputOrth) [onClick AppRefreshI]
      , spacer
      , optionButton_ "Grubb" IGrubb (inputOrth) [onClick AppRefreshI]
      , spacer
      , optionButton_ "Boas" IBoas (inputOrth) [onClick AppRefreshI]
      , spacer
      , optionButton_ "Georgian" IGeorgian (inputOrth) [onClick AppRefreshI]
      ]
    , spacer
    , hstack
      [ label "Output" `styleBasic` [textFont "Monotype"]
      , spacer
      , optionButton_ "U'mista" OUmista (outputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "NAPA" ONapa (outputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "Grubb" OGrubb (outputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "Boas" OBoas (outputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "Georgian" OGeorgian (outputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "IPA" OIpa (outputOrth) [onClick AppRefresh]
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
        [ button "Choose Destination" AppSaveFile
        , spacer
        , (textField_ outputFile [readOnly])
        , spacer
        , (textArea_ outputText [readOnly]) `styleBasic` [textFont $ selectFontO $ model ^. outputOrth]
        ]
      ]
    , spacer
    , button "Save File" AppWriteFile
    , popup overwriteConfVis (confirmMsg "File already Exists. Overwrite?" AppOverWrite AppClosePopups)
    , popup errorAlertVis (alertMsg (model ^. errorMsg) AppClosePopups)
    , popup writeSuccessVis (alertMsg "File Saved Successfully." AppClosePopups)
    ] `styleBasic` [padding 10]

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
  (AppSetInput  fnm) -> [Model (model & inputFile  .~ fnm), Task $ AppGotInput <$> TU.readFile (T.unpack fnm)]
  (AppSetOutput fnm) -> [Model (model & outputFile .~ fnm)]
  AppOpenFile -> [Task $ handleFile1 <$> openFileDialog "Open Input File" "" ["*.txt", "*.*"] "Text Files" False]
  AppSaveFile -> [Task $ handleFile2 <$> saveFileDialog "Select Output File" (model ^. inputFile) ["*.txt", "*.*"] "Text Files"]
  AppGotInput txt -> [Model (model & inputText .~ txt & outputText .~ getConversion txt)]
  AppWriteFile -> [Task $ writeFileTask (T.unpack (model ^. outputFile)) (model ^. outputText)] 
  AppWriteSuccess -> [Model (model & writeSuccessVis .~ True)] -- Display a pop-up message, maybe?
  AppWriteExists -> [Model (model & overwriteConfVis .~ True)]
  (AppWriteError err) -> [Model (model & errorMsg .~ (renderError err) & errorAlertVis .~ True)]
  AppOverWrite -> [Task $ overWriteFileTask (T.unpack (model ^. outputFile)) (model ^. outputText), Model (model & overwriteConfVis .~ False)] 
  AppRefresh  -> [Model (model & outputText .~ getConversion (model ^. inputText))]
  AppRefreshI -> [Model (model & outputText .~ getConversion (model ^. inputText) & inputText %~ modText)]
  AppClosePopups -> [Model (model & overwriteConfVis .~ False & errorAlertVis .~ False & writeSuccessVis .~ False)]
  (AppCurDir fp) -> [Model (model & currentDir .~ (T.pack fp))]
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
      in decodeKwakwalaD outO $ parseKwakwalaD inpO inpTxt
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
    modText :: Text -> Text
    modText txt = case (T.unsnoc txt) of
      Nothing -> " "
      Just (txt', ' ') -> txt'
      Just (_txt,  _ ) -> (snoc txt ' ')

writeFileTask :: FilePath -> Text -> IO AppEvent
writeFileTask fp txt = do
  bl <- doesFileExist fp
  if bl
    then return AppWriteExists
    else do 
      eEvt <- try @SomeException (TU.writeFile fp txt)
      case eEvt of
        Left x   -> return (AppWriteError $ T.pack (show x))
        Right () -> return AppWriteSuccess


overWriteFileTask :: FilePath -> Text -> IO AppEvent
overWriteFileTask fp txt = do
  eEvt <- try @SomeException (TU.writeFile fp txt)
  case eEvt of
    Left x   -> return (AppWriteError $ T.pack (show x))
    Right () -> return AppWriteSuccess

-- KurintoSansAux-Rg.ttf

selectFontI :: InputOrth -> Font
selectFontI IUmista   = "Umista"
selectFontI INapa     = "NAPA"
selectFontI IGrubb    = "Universal"
selectFontI IGeorgian = "Georgian"
selectFontI IBoas     = "Boas"

selectFontO :: OutputOrth -> Font
selectFontO OUmista   = "Umista"
selectFontO ONapa     = "NAPA"
selectFontO OGrubb    = "Universal"
selectFontO OGeorgian = "Georgian"
selectFontO OBoas     = "Boas"
selectFontO OIpa      = "IPA"  

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Kwak'wala Orthography Conversion (File)",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      -- appFontDef "Universal" "./assets/fonts/LiberationSans-Regular.ttf",
      -- appFontDef "Universal" "./assets/fonts/KurintoSansAux-Rg.ttf",
      appFontDef "Monotype" "./assets/fonts/LiberationMono-Regular.ttf",
      appFontDef "Universal" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "Georgian" "./assets/fonts/NotoSansGeorgian-Regular.ttf",
      appFontDef "Umista" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "NAPA" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "Boas" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "IPA" "./assets/fonts/DoulosSIL-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel IUmista OUmista "" "" "" "" "" False False False ""
