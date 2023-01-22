{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

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
  | AppNull -- Empty event
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
      , optionButton "U'mista" IUmista (inputOrth)
      , spacer
      , optionButton "NAPA" INapa (inputOrth)
      , spacer
      , optionButton "Grubb" IGrubb (inputOrth)
      , spacer
      , optionButton "Boas" IBoas (inputOrth)
      , spacer
      , optionButton "Georgian" IGeorgian (inputOrth)
      ]
    , spacer
    , hstack
      [ label "Input:"
      , spacer
      , optionButton "U'mista" OUmista (outputOrth)
      , spacer
      , optionButton "NAPA" ONapa (outputOrth)
      , spacer
      , optionButton "Grubb" OGrubb (outputOrth)
      , spacer
      , optionButton "Boas" OBoas (outputOrth)
      , spacer
      , optionButton "Georgian" OGeorgian (outputOrth)
      , spacer
      , optionButton "IPA" OIpa (outputOrth)
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
  AppInit -> []
  AppNull -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  (AppSetInput  fnm) -> [Model (model & inputFile  .~ fnm), Task $ AppGotInput <$> TU.readFile (T.unpack fnm)]
  (AppSetOutput fnm) -> [Model (model & outputFile .~ fnm)]
  AppOpenFile -> [Task $ handleFile1 <$> openFileDialog "Open Input File" "%HOME%" [] "Text Files" False]
  AppSaveFile -> [Task $ handleFile2 <$> saveFileDialog "Select Output File" "%HOME%" [] "Text Files"]
  AppGotInput txt -> [Model (model & inputText .~ txt & outputText .~ getConversion txt)]
  AppWriteFile -> [] -- for now
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
    model = AppModel 0 IUmista OUmista "" "" "" ""
