{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Lens
import Data.Default (def)
import Data.Ini.Config.Bidir
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Monomer
import TextShow

import Kwakwala.GUI.Config
import Kwakwala.GUI.Config.File
import Kwakwala.GUI.Config.Parsing
import Kwakwala.GUI.Info
import Kwakwala.GUI.Types

import qualified Monomer.Lens as L

import Monomer.Widgets.Containers.Scroll

import Monomer.Widgets.Singles.OptionButton
import Monomer.Widgets.Singles.TextArea

-- from kwak-orth
import Kwakwala.Sounds

import System.Directory
import System.FilePath

import TextUTF8 qualified as TU

data AppModel = AppModel 
  { _inputOrth   :: InputOrth
  , _outputOrth  :: OutputOrth
  , _inputText   :: Text
  , _outputText  :: Text
  , _autoConvert :: Bool
  , _lastOutput  :: OutputOrth
  , _configVis   :: Bool
  , _errorAlertVis :: Bool
  , _errorMsg    :: Text
  , _kwakConfig  :: KwakConfigModel
  , _cfgFilePath :: FilePath
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppNull
  | AppConvert
  | AppChange -- | When the input text box changes.
  | AppSwap
  | AppOpenConfig
  | AppDoneConfig
  | AppClosePopups
  | AppError Text
  -- | AppConfigSuccess
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
      , filler
      , button "Config" AppOpenConfig
      ]
    , spacer
    , popup_ configVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $
        vstack
          [ kwakConfigWidgetX kwakConfig
          , button "Done" AppDoneConfig
          ] `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]
    , spacer
    , hstack
      [ label "Input " `styleBasic` [textFont "Monotype"]
      , spacer
      , tooltipK ttUmista   $ optionButton_ "U'mista" IUmista (inputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttNapa     $ optionButton_ "NAPA" INapa (inputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttGrubb    $ optionButton_ "Grubb" IGrubb (inputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttBoas     $ optionButton_ "Boas" IBoas (inputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttGeorgian $ optionButton_ "Georgian" IGeorgian (inputOrth) [onClick AppChange]
      ]
    , spacer
    , hstack
      [ label "Output" `styleBasic` [textFont "Monotype"]
      , spacer
      , tooltipK ttUmista   $ optionButton_ "U'mista" OUmista (outputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttNapa     $ optionButton_ "NAPA" ONapa (outputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttGrubb    $ optionButton_ "Grubb" OGrubb (outputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttBoas'    $ optionButton_ "Boas" OBoas (outputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttGeorgian $ optionButton_ "Georgian" OGeorgian (outputOrth) [onClick AppChange]
      , spacer
      , tooltipK ttIpa      $ optionButton_ "IPA" OIpa (outputOrth) [onClick AppChange]
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
    , popup errorAlertVis (alertMsg (model ^. errorMsg) AppClosePopups) `styleBasic` [textFont "Monotype"]
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
  AppOpenConfig  -> [Model (model & configVis .~ True )]
  AppDoneConfig  -> [Model (model & configVis .~ False), Task $ writeConfigTask (model ^. cfgFilePath) (model ^. kwakConfig)] -- temp
  AppClosePopups -> [Model (model & errorAlertVis .~ False)]
  (AppError err) -> [Model (model & errorAlertVis .~ True & errorMsg .~ err)]

-- KurintoSansAux-Rg.ttf

writeConfigTask :: FilePath -> KwakConfigModel -> IO AppEvent
writeConfigTask fp kcm = do
  rslt <- updateConfigFile' fp kcm
  case rslt of
    Just err -> return $ AppError ("Error writing config file:\n" <> err)
    Nothing  -> return AppNull

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
  (cfgFile, eConf) <- findAndCreateConf
  startApp (model' cfgFile eConf) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Kwak'wala Orthography Conversion (Text)",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Monotype" "./assets/fonts/LiberationMono-Regular.ttf",
      appFontDef "Universal" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "Georgian" "./assets/fonts/NotoSansGeorgian-Regular.ttf",
      appFontDef "Umista" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "NAPA"  "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "Boas" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "IPA" "./assets/fonts/DoulosSIL-Regular.ttf",
      appInitEvent AppInit
      ]
    -- model  = AppModel IUmista OUmista "" "" False OUmista False False "" def fp
    model' cfgFile (Left   txt) = AppModel IUmista OUmista "" "" False OUmista False True txt def cfgFile
    model' cfgFile (Right iniX) = AppModel IUmista OUmista "" "" False OUmista False False ""  (getIniValue iniX) cfgFile
