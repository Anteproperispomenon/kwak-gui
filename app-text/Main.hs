{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  { _clickCount  :: Int
  , _inputOrth   :: InputOrth
  , _outputOrth  :: OutputOrth
  , _inputText   :: Text
  , _outputText  :: Text
  , _autoConvert :: Bool
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppConvert
  | AppChange -- | When the input text box changes.
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack 
    [ labeledCheckbox "Auto-Convert Text" autoConvert
    , spacer
    , hstack
      [ label "Input " `styleBasic` [textFont "Monotype"]
      , spacer
      , optionButton_ "U'mista" IUmista (inputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "NAPA" INapa (inputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "Grubb" IGrubb (inputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "Boas" IBoas (inputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "Georgian" IGeorgian (inputOrth) [onClick AppChange]
      ]
    , spacer
    , hstack
      [ label "Output" `styleBasic` [textFont "Monotype"]
      , spacer
      , optionButton_ "U'mista" OUmista (outputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "NAPA" ONapa (outputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "Grubb" OGrubb (outputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "Boas" OBoas (outputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "Georgian" OGeorgian (outputOrth) [onClick AppChange]
      , spacer
      , optionButton_ "IPA" OIpa (outputOrth) [onClick AppChange]
      ]
    , spacer
    , hgrid_ [childSpacing_ 8]
      [ vstack
        [ box_ [alignCenter] $ label "Input"
        , (textArea_  inputText [onChange (\(_txt :: Text) -> AppChange)])  `styleBasic` [textFont $ selectFontI $ model ^. inputOrth]
        ]
      , vstack
        [ box_ [alignCenter] $ label "Output"
        , (textArea_ outputText [readOnly]) `styleBasic` [textFont $ selectFontO $ model ^. outputOrth]
        ]
      ]
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
  AppChange ->
    case (model ^. autoConvert) of
      True  -> 
        let txt1 = model ^. inputText
            inpO = model ^. inputOrth
            outO = model ^. outputOrth
            txt2 = decodeKwakwalaD outO $ parseKwakwalaD inpO txt1
        in [Model (model & outputText .~ txt2)]
      False -> []

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
      appWindowTitle "Hello world",
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
    model = AppModel 0 IUmista OUmista "" "" False
