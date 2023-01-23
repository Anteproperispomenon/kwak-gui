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
  { _clickCount :: Int
  , _inputOrth  :: InputOrth
  , _outputOrth :: OutputOrth
  , _inputFile :: Text
  , _outputFile :: Text
  , _inputText :: Text
  , _outputText :: Text
  , _currentDir :: Text -- Current Working Directory
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppSetInput Text
  | AppSetOutput Text
  | AppOpenFile -- triggers dialog
  | AppSaveFile -- triggers dialog
  | AppWriteFile -- Actually writes content to file
  | AppGotInput Text
  | AppRefresh -- Refresh Output
  | AppCurDir FilePath
  | AppNull -- Empty event
  | AppWriteSuccess
  | AppWriteExists
  | AppWriteError Text
  | AppOverWrite -- when the user agrees to overwrite a file.
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack 
    [ label "Hello world"
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
    , spacer
    , hstack
      [ label "Input:"
      , spacer
      , optionButton_ "U'mista" IUmista (inputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "NAPA" INapa (inputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "Grubb" IGrubb (inputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "Boas" IBoas (inputOrth) [onClick AppRefresh]
      , spacer
      , optionButton_ "Georgian" IGeorgian (inputOrth) [onClick AppRefresh]
      ]
    , spacer
    , hstack
      [ label "Input:"
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
    , button "Select File" AppOpenFile
    , dropTarget (\txt -> AppSetInput txt) (textField_ inputFile [readOnly])
    , spacer
    , vscroll $ (textArea_ inputText [readOnly]) `styleBasic` [textFont "Universal"]
    , spacer
    , button "Choose Destination" AppSaveFile
    , (textField_ outputFile [readOnly])
    , spacer
    , vscroll $ (textArea_ outputText [readOnly]) `styleBasic` [textFont "Universal"]
    , spacer
    , button "Save File" AppWriteFile
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
  AppIncrease -> [Model (model & clickCount +~ 1)]
  (AppSetInput  fnm) -> [Model (model & inputFile  .~ fnm), Task $ AppGotInput <$> TU.readFile (T.unpack fnm)]
  (AppSetOutput fnm) -> [Model (model & outputFile .~ fnm)]
  AppOpenFile -> [Task $ handleFile1 <$> openFileDialog "Open Input File" "" [] "Text Files" False]
  AppSaveFile -> [Task $ handleFile2 <$> saveFileDialog "Select Output File" inpDir [] "Text Files"]
  AppGotInput txt -> [Model (model & inputText .~ txt & outputText .~ getConversion txt)]
  AppWriteFile -> [Task $ writeFileTask (T.unpack (model ^. outputFile)) (model ^. outputText)] 
  AppWriteSuccess -> [] -- Display a pop-up message, maybe?
  AppWriteExists -> []
  (AppWriteError err) -> []
  AppOverWrite -> [Task $ overWriteFileTask (T.unpack (model ^. outputFile)) (model ^. outputText)] 
  AppRefresh -> [Model (model & outputText .~ getConversion (model ^. inputText))]
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

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      -- appFontDef "Universal" "./assets/fonts/LiberationSans-Regular.ttf",
      -- appFontDef "Universal" "./assets/fonts/KurintoSansAux-Rg.ttf",
      appFontDef "Universal" "./assets/fonts/KurintoSans-Rg.ttf",
      appInitEvent AppInit
      ]
    model = AppModel 0 IUmista OUmista "" "" "" "" ""
