{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import Kwakwala.GUI.Types

import qualified Monomer.Lens as L

import Monomer.Widgets.Containers.Scroll

import Monomer.Widgets.Singles.OptionButton
import Monomer.Widgets.Singles.TextArea

-- from kwak-orth
import Kwakwala.Sounds

data AppModel = AppModel 
  { _clickCount :: Int
  , _inputOrth  :: InputOrth
  , _outputOrth :: OutputOrth
  , _inputText  :: Text
  , _outputText :: Text
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppConvert
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
    , label "Input"
    , vscroll $ (textArea inputText) `styleBasic` [textFont "Universal"]
    , spacer
    , label "Output"
    , vscroll $ (textArea_ outputText [readOnly]) `styleBasic` [textFont "Universal"]
    , spacer
    , button "Convert" AppConvert
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  AppConvert -> 
    let txt1 = model ^. inputText
        inpO = model ^. inputOrth
        outO = model ^. outputOrth
        txt2 = decodeKwakwalaD outO $ parseKwakwalaD inpO txt1
    in [Model (model & outputText .~ txt2)]

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
    model = AppModel 0 IUmista OUmista "" ""
