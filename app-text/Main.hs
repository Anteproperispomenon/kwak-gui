{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Default (def)
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import Kwakwala.GUI.Config
import Kwakwala.GUI.Types

import qualified Monomer.Lens as L

import Monomer.Widgets.Containers.Scroll

import Monomer.Widgets.Singles.OptionButton
import Monomer.Widgets.Singles.TextArea

-- from kwak-orth
import Kwakwala.Sounds

data AppModel = AppModel 
  { _inputOrth   :: InputOrth
  , _outputOrth  :: OutputOrth
  , _inputText   :: Text
  , _outputText  :: Text
  , _autoConvert :: Bool
  , _lastOutput  :: OutputOrth
  , _kwakConfig  :: KwakConfigModel
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppConvert
  | AppChange -- | When the input text box changes.
  | AppSwap
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack 
    [ hstack
      [ labeledCheckbox "Auto-Convert Text" autoConvert
      , spacer
      , button "\x21CB Swap Orthographies" AppSwap `styleBasic` [textFont "Universal"]
      ]
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
        , spacer
        , buttonD_ "Copy to Clipboard" [onClickReq (SetClipboard (ClipboardText $ model ^. outputText))]
        ] -- don't add 'onChange' to this textArea.
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
  -- AppIncrease -> [Model (model & clickCount +~ 1)]
  AppConvert -> 
    let txt1 = model ^. inputText
        inpO = model ^. inputOrth
        outO = model ^. outputOrth
        kcm  = model ^. kwakConfig
        txt2 = decodeKwakwalaD kcm outO $ parseKwakwalaD kcm inpO txt1
    in [Model (model & outputText .~ txt2 & lastOutput .~ outO)]
  AppChange ->
    case (model ^. autoConvert) of
      True  -> 
        let txt1 = model ^. inputText
            inpO = model ^. inputOrth
            outO = model ^. outputOrth
            kcm  = model ^. kwakConfig
            txt2 = decodeKwakwalaD kcm outO $ parseKwakwalaD kcm inpO txt1
        in [Model (model & outputText .~ txt2 & lastOutput .~ outO)]
      False -> []
  AppSwap ->
    case (model ^. lastOutput) of
      OIpa -> [] -- maybe add error message
      orth -> 
        let 
          txtO = model ^. outputText
          ortO = model ^. lastOutput
          ortI = model ^. inputOrth
          ortO' = orthO2I ortO -- Maybe InputOrth
          ortI' = orthI2O ortI -- OutputOrth
          kcm   = model ^. kwakConfig
          txtI = (\ort -> (decodeKwakwalaD kcm ortI' $ parseKwakwalaD kcm ort txtO, ort)) <$> ortO'
        in case txtI of
          Nothing        -> []
          Just (txt,ort) -> [Model (model & outputText .~ txt & inputText .~ txtO & inputOrth .~ ort & outputOrth .~ ortI' & lastOutput .~ ortI')]

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
      appWindowTitle "Kwak'wala Orthography Conversion (Text)",
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
    model = AppModel IUmista OUmista "" "" False OUmista def
