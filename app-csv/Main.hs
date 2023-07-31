{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Exception
import Control.Lens
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Default (def)
import Data.Ini.Config.Bidir
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Monomer
import Monomer.Common.BasicTypes
import TextShow

-- import Data.Array qualified as Arr
import Data.IntMap.Strict qualified as IM

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

import Text.CSV.Lazy.ByteString

-- from kwak-orth
import Kwakwala.Sounds

data AppModel = AppModel 
  { _inputFile  :: Text
  , _outputFile :: Text
  -- , _inputHeaders :: [Text]
  , _inputText  :: IM.IntMap (Maybe Text, Bool, InputOrth, OutputOrth, Text, Text)
  -- , _outputText :: Text
  , _currentDir :: Text -- Current Working Directory
  , _readHeaders :: Bool
  , _overwriteConfVis :: Bool
  , _errorAlertVis :: Bool
  , _writeSuccessVis :: Bool
  , _openErrorVis :: Bool
  , _configVis :: Bool
  , _sfmVis    :: Bool
  , _csvVis    :: Bool
  , _saveVis   :: Bool
  , _copyVis   :: Bool
  , _copyIO    :: Bool
  , _delCfmVis :: Bool
  , _nextKey   :: Int
  , _copyKey   :: Int
  , _newHeader :: Text
  , _errorMsg  :: Text
  , _kwakConfig :: KwakConfigModel
  , _sfmHidden :: HiddenVal SaveFileModel
  , _cfgFilePath :: FilePath
  , _csvSep :: Maybe Char
  -- , _csvHeaders :: [Bool]
  -- , _csvSelection :: Int -- Which column is being worked on.
  -- , _kwakConfig :: Ini KwakConfigModel
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSetInput Text
  | AppSetInput2
  | AppSetOutput Text
  | AppOpenFile -- triggers dialog
  | AppOpenFileKey -- when using the keystroke
  | AppSaveFile -- triggers dialog
  | AppSaveFileKey -- when using the keystroke
  | AppSaveFileMan -- triggers manual output
  | AppWriteFile -- Actually writes content to file
  | AppWriteFileKey
  | AppGotInput ([CSVError],[(Maybe Text, Bool, InputOrth, OutputOrth, Text, Text)])
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
  | AppChangeIOrth Int  InputOrth
  | AppChangeOOrth Int OutputOrth
  | AppChangeModify Int Bool
  | AppCheckOverwrite Bool
  | AppStartCopy Int
  | AppCopyColumn Int
  | AppDeleteColumn  Int
  | AppDeleteColumn2 Int
  | AppSwapColumn Int Int
  | AppError Text
  deriving (Eq, Show)


makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = keystroke keyCommands $ vstack 
    [ button "Config" AppOpenConfig
    , spacer
    , popup_ configVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $ 
        box $ vstack
          -- [ kwakConfigWidgetX (kwakConfig . iniValueL)
          [ kwakConfigWidgetX kwakConfig
          , button "Done" AppDoneConfig
          ] `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]

    --------------------------------
    -- Select save file 
    , popup_ sfmVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $ 
        box (selectSaveFileH sfmHidden inputFile convertSFEvent)
          `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]
    
    --------------------------------
    -- Open file options
    , popup_ csvVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $
        box $ vstack
          [ label "CSV Input Settings" `styleBasic` [textSize 24, textCenter]
          , spacer
          , box $ hstack $
             [ label "Separator"
             , spacer
             , optionButton "," Nothing csvSep 
             , spacer
             , optionButton ";" (Just ';') csvSep
             , spacer
             , optionButton "Tab" (Just '\t') csvSep
             ]
          , spacer
          -- The tooltip doesn't work. May be because it is embedded in a popup.
          , tooltipFK headerInTT $ box $ labeledCheckbox "Get Headers from File" readHeaders
          -- , tooltipK headerInTT $ toggleButton "Get Headers from File" readHeaders
          , spacer
          , button "Import Data" AppSetInput2 -- don't have the model, but that's okay.
          , spacer
          , button "Cancel" AppClosePopups
          ] `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]
    
    --------------------------------
    -- Save Dialog
    , popup_ saveVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $
        box $ vstack
          [ label "CSV Output Settings" `styleBasic` [textSize 24, textCenter]
          , spacer
          , box $ hstack $
             [ label "Separator"
             , spacer
             , optionButton "," Nothing csvSep 
             , spacer
             , optionButton ";" (Just ';') csvSep
             , spacer
             , optionButton "Tab" (Just '\t') csvSep
             ]
          , spacer
          -- The tooltip doesn't work. May be because it is embedded in a popup.
          , tooltipFK headerOutTT $ labeledCheckbox "Save Headers to File" readHeaders
          , spacer
          , button "Save File" AppWriteFile
          , spacer
          , button "Cancel" AppClosePopups
          ] `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]

    --------------------------------
    -- Confirm Delete
    
    , popup_ delCfmVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $ 
        box $ vstack
          [ label_ ("Are you sure you want\nto delete this column?" ) [multiline] `styleBasic` [textSize 20, textCenter]
          , spacer
          , hstack $
             [ label "Column"
             , spacer
             , textFieldV_ (getHeaderText model (model ^. copyKey)) (\_ -> AppNull) [readOnly]
             ]
          , filler
          , button "Delete" (AppDeleteColumn2 (model ^. copyKey))
          , spacer
          , button "Cancel" AppClosePopups
          ]
          `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]

    --------------------------------
    -- Copy Dialog
    , popup_ copyVis [popupAlignToWindow, alignTop, alignCenter, popupOffset (def {_pY = 30}), popupDisableClose] $
        box $ vstack
          [ label "Copy Column" `styleBasic` [textSize 24, textCenter]
          , spacer
          , hstack $
             [ label "Copy from"
             , spacer
             , optionButton "Input" False copyIO
             , spacer
             , optionButton "Output" True copyIO
             ]
          , spacer
          , hstack $
             [ label "Header: "
             , spacer
             , textField newHeader
             ]
          , spacer
          , button "Copy Column" (AppCopyColumn (model ^. copyKey))
          , spacer
          , button "Cancel" AppClosePopups
          ] `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]

    , button "Select File" AppOpenFile
    , spacer

    {- -- maybe re-open
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
      
      ]-}
    , hscroll $ spreadColumns (model ^. inputText)
    , spacer
    , button "Save File" AppSaveFile
    , popup overwriteConfVis (confirmMsg "File already Exists. Overwrite?" AppOverWrite AppClosePopups)
    , popup errorAlertVis (altAlertMsg_ (model ^. errorMsg) AppClosePopups [titleCaption "Error"]) `styleBasic` [textFont "Monotype"]
    , popup writeSuccessVis (alertMsg "File Saved Successfully." AppClosePopups)
    , popup openErrorVis (alertMsg "Could not open requested file." AppClosePopups)
    ] `styleBasic` [padding 10]
  keyCommands
    = [ ("Ctrl-o", AppOpenFileKey )
      , ("Cmd-o" , AppOpenFileKey )
      , ("Ctrl-s", AppSaveFileKey )
      , ("Cmd-s" , AppSaveFileKey )
      , ("Ctrl-w", AppWriteFileKey)
      , ("Cmd-w" , AppWriteFileKey)
      ]
  -- Maybe in the future use model parameters
  -- to change how the columns are rendered, e.g.
  -- 
  spreadColumns :: IM.IntMap (Maybe Text, Bool, InputOrth, OutputOrth, Text, Text) -> WidgetNode AppModel AppEvent
  spreadColumns strs = hgrid_ [childSpacing_ 2] $ forWithKey strs $ \key (hdr, cvtble, iorth, oorth, itxt, otxt) ->
    dropTarget (\n -> AppSwapColumn n key) $ vstack $
      [ label $ fromMaybe ("Column " <> showt key) hdr
      , spacer
      , draggable key $ box (label "Swap Column") `styleBasic` [bgColor darkSlateGray, padding 3, border 1 slateGray, radius 3]
      , spacer
      , tooltipK modifyTT $ labeledCheckboxV "Modify?" cvtble (\bl -> AppChangeModify key bl)
      , spacer
      , label "Input"
      , textDropdownV iorth (\io -> AppChangeIOrth key io) [IUmista, INapa, IGrubb, IGeorgian, IBoas, IIsland]
      , spacer
      , label "Output"
      , textDropdownV oorth (\oo -> AppChangeOOrth key oo) [OUmista, ONapa, OGrubb, OGeorgian, OBoas, OIsland, OIpa]
      , spacer
      , hgrid_ [childSpacing_ 2] $ 
         [ button "Copy" (AppStartCopy key)
         , button "Delete" (AppDeleteColumn key)
         ]
      , spacer
      , textAreaV_ otxt (\_ -> AppNull) [readOnly] 
          `styleBasic` [textFont $ selectFontO $ oorth]
      ]
  modifyTT :: Text
  modifyTT = "Whether to apply an orthography conversion to this column.\nLeave this unchecked for e.g. English columns."  
  headerInTT :: Text
  headerInTT = 
    "If selected, use the first entry in each column as the header\n"
      <> "for that column. Otherwise, just use generic header names.\n"
      <> "Leave this unselected if you want to convert the header names\n"
      <> "to other orthographies."
  headerOutTT :: Text
  headerOutTT = 
    "If selected, save the header names as the first entry in each column.\n"
      <> "In general, use the same setting for this as you did when importing."

  {-
  spreadColumns :: [String] -> [Text] -> WidgetNode AppModel AppEvent
  spreadColumns hdrs txts
    = hgrid $ zipWith3L hdrs [0..]  $ \hdr txt n ->
        vstack $
          [ optionButton $ T.pack hdr
          , textAreaV_ txt (\_ -> AppNull) [readOnly]
          ]
  -}

getHeaderText :: AppModel -> Int -> Text
getHeaderText model ky
  | Nothing <- rslt
  = "<error>"
  | Just (Nothing, _, _, _, _, _) <- rslt
  = "Column #" <> showt ky
  | Just (Just txt, _, _, _, _, _) <- rslt
  = txt
  where rslt = IM.lookup ky (model ^. inputText)

forWithKey :: IM.IntMap a -> (Int -> a -> b) -> IM.IntMap b
forWithKey = flip IM.mapWithKey

forL :: [a] -> (a -> b) -> [b]
forL xs f = map f xs

zipWithL :: [a] -> [b] -> (a -> b -> c) -> [c]
zipWithL l1 l2 f = zipWith f l1 l2

zipWith3L :: [a] -> [b] -> [c] -> (a -> b -> c -> d) -> [d]
zipWith3L l1 l2 l3 f = zipWith3 f l1 l2 l3

sizeReqX :: (SizeReq, SizeReq) -> (SizeReq, SizeReq)
sizeReqX (szrW, szrH)
  = (szrW {_szrFlex = 5, _szrExtra = 5, _szrFactor = 0.005}, szrH)

{-
  | AppChangeIOrth Int  InputOrth
  | AppChangeOOrth Int OutputOrth
  | AppChangeModify Int Bool
-}

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
-- handleEvent wenv node model evt = []
handleEvent wenv node model evt = case evt of
  AppInit -> [Task $ AppCurDir <$> getCurrentDirectory]
  AppNull -> []
  (AppSetInput  fnm) -> [Model (model & inputFile  .~ fnm & csvVis .~ True)]
  (AppSetInput2    ) -> let fnm = (model ^. inputFile) in [(Task $ AppGotInput <$> readCSVMaybe (model ^. readHeaders) (model ^. csvSep) (T.unpack fnm)), (Model (model & csvVis .~ False))]
  (AppSetOutput fnm) -> [Model (model & outputFile .~ fnm & sfmVis .~ False), Task $ AppCheckOverwrite <$> doesFileExist (T.unpack fnm)] -- & saveVis .~ True)]
  (AppCheckOverwrite ovr) ->
    if ovr
      then [Model (model & overwriteConfVis .~ True)]
      else [Model (model & saveVis .~ True)]
  AppOverWrite -> [Model (model & overwriteConfVis .~ False & saveVis .~ True)]
  (AppChangeIOrth ky io) 
    -> [Model 
         (model & inputText . at ky %~ \case
           Nothing -> Nothing
           Just (hdr, True , oldIo, oldOo, itxt, otxt) -> Just (hdr, True , io, oldOo, itxt, convertText io oldOo itxt)
           Just (hdr, False, oldIo, oldOo, itxt, otxt) -> Just (hdr, False, io, oldOo, itxt, otxt)
         )
       ]
  (AppChangeOOrth ky oo) 
    -> [Model 
         (model & inputText . at ky %~ \case
           Nothing -> Nothing
           Just (hdr, True , oldIo, oldOo, itxt, otxt) -> Just (hdr, True , oldIo, oo, itxt, convertText oldIo oo itxt)
           Just (hdr, False, oldIo, oldOo, itxt, otxt) -> Just (hdr, False, oldIo, oo, itxt, otxt)
         )
       ]
  (AppChangeModify ky True) 
    -> [Model 
         (model & inputText . at ky %~ \case
           Nothing -> Nothing
           Just (hdr, oldbl, io, oo, itxt, otxt) -> Just (hdr, True, io, oo, itxt, convertText io oo itxt)
         )
       ]
  (AppChangeModify ky False) 
    -> [Model 
         (model & inputText . at ky %~ \case
           Nothing -> Nothing
           Just (hdr, oldbl, io, oo, itxt, otxt) -> Just (hdr, False, io, oo, itxt, itxt)
         )
       ]

  AppOpenFile -> [Task $ handleFile1 <$> openFileDialog "Open Input File" "" ["*.csv", "*.tsv", "*.*"] "CSV Files" False]
  AppGotInput (errs, cols) -> 
    let colMap = (IM.fromList (zip [1..] cols))
        mmk    = IM.lookupMax colMap
        (mk,_) = fromMaybe (1,undefined) mmk -- should be safe?
        model' = (model & inputText .~ colMap & nextKey .~ (1+mk)) -- to get to next key
    in case errs of
      [] -> [Model model']
      es -> [Model (model' & openErrorVis .~ True & errorMsg .~ (T.pack $ unlines $ map ppCSVError es))]
  AppClosePopups -> [Model 
    (model 
      & overwriteConfVis .~ False
      & errorAlertVis .~ False
      & writeSuccessVis .~ False
      & openErrorVis .~ False
      & configVis .~ False
      & sfmVis .~ False
      & csvVis .~ False
      & saveVis .~ False
      & copyVis .~ False
      & delCfmVis .~ False
      )]
-- writeFileTask :: FilePath -> FilePath -> Bool -> Char -> IM.IntMap (Maybe Text, Bool, InputOrth, OutputOrth, Text, Text) -> AppEventResponse AppModel AppEvent
  AppWriteFile -> 
    [writeFileTask 
      (T.unpack (model ^. inputFile)) 
      (T.unpack (model ^. outputFile)) 
      (model ^. readHeaders) 
      (fromMaybe ',' (model ^. csvSep)) 
      (model ^. inputText)
    , Model (model & saveVis .~ False)
    ]
  AppSaveFile -> [Task $ handleFile2 <$> saveFileDialog "Select Output File" (model ^. inputFile) ["*.csv", "*.tsv", "*.*"] "CSV Files"]

  -- (Maybe Text, Bool, InputOrth, OutputOrth, Text, Text)

  (AppStartCopy  n) -> 
    [ Model 
      (model 
        & copyVis .~ True
        & copyKey .~ n
        -- & 
      )
    ]

  (AppCopyColumn n) -> 
    case (IM.lookup n (model ^. inputText)) of
      Nothing -> [Event $ AppError $ "Couldn't lookup column #" <> showt n <> "."]
      Just (mhdrx, mdfy, iorth, oorth, itxt, otxt) ->
        let takFrom = model ^. copyIO
            (theTxt, outTxt, io', oo') 
              = if | (not mdfy)      -> (itxt, itxt, iorth, orthI2O iorth)
                   | (not takFrom)   -> (itxt, itxt, iorth, orthI2O iorth)
                   | (oorth == OIpa) -> (itxt, otxt, iorth, OIpa)
                   | otherwise       -> (otxt, otxt, fromJust $ orthO2I oorth, oorth)
            newHdr = model ^. newHeader
            newEntry = (nothifyText newHdr, mdfy, io', oo', theTxt, outTxt)
            newKey = model ^. nextKey
        in [ Model 
             (model 
               & copyVis .~ False
               & nextKey +~ 1
               & inputText %~ IM.insert newKey newEntry
               & newHeader .~ ""
             )
           ]
  
  (AppSwapColumn m n) ->
    if (m == n)
      then []
      else let itxt = model ^. inputText
           in case (IM.lookup m itxt, IM.lookup n itxt) of
                (Nothing, _) -> []
                (_, Nothing) -> []
                (rsltM, rsltN) ->
                  [ Model 
                      (model
                        & inputText . at n .~ rsltM
                        & inputText . at m .~ rsltN
                      )
                  ]

  (AppDeleteColumn  n) -> [ Model (model & delCfmVis .~ True & copyKey .~ n)]
  (AppDeleteColumn2 n) -> 
    [ Model
        (model
          & delCfmVis .~ False
          & inputText %~ IM.delete n
        )

    ]

  AppOpenConfig -> [Model (model & configVis .~ True )]
  AppDoneConfig -> [Event AppRefresh, Model (model & configVis .~ False), Task $ writeConfigTask (model ^. cfgFilePath) (model ^. kwakConfig)]

  (AppError err) -> [Model (model & errorAlertVis .~ True & errorMsg .~ err)]

  AppOpenFileKey  -> if checkNoPopups then [Event AppOpenFile]  else []
  AppSaveFileKey  -> if checkNoPopups then [Event AppSaveFile]  else []
  {-
  AppSaveFileMan  -> [Model (model & sfmVis .~ True)]
  AppWriteFileKey -> if checkNoPopups then [Event AppWriteFile] else []
  AppGotInput mtxt -> case mtxt of
     (Just txt) -> [Model (model & inputText .~ txt & outputText .~ getConversion txt)]
     Nothing    -> [Model (model & openErrorVis .~ True)]
  -- Technically not a task anymore; it's just a bit too complicated to fit here.
  -- _ -> []
  AppWriteSuccess -> [Model (model & writeSuccessVis .~ True)] -- Display a pop-up message, maybe?
  AppWriteExists -> [Model (model & overwriteConfVis .~ True)]
  (AppWriteError err) -> [Model (model & errorMsg .~ (renderError err) & errorAlertVis .~ True), tlogErrTask err]
  AppOverWrite -> [Task $ overWriteFileTask (T.unpack (model ^. outputFile)) (model ^. outputText), Model (model & overwriteConfVis .~ False)] 
  AppRefresh  -> [Model (model & outputText .~ getConversion (model ^. inputText))]
  AppRefreshI -> [Model (model & outputText .~ getConversion (model ^. inputText) & inputText %~ modText)]
  (AppCurDir fp) -> [Model (model & currentDir .~ (T.pack fp))]
  -- AppDoneConfig -> let newCfg = selfUpdate (model ^. kwakConfig)
  --   in [Model (model & configVis .~ False & kwakConfig .~ newCfg)] -- Add task here to update config file.
  
  -}
  _ -> []
  where 
    checkNoPopups :: Bool
    checkNoPopups = not ((model ^. overwriteConfVis)
      || (model ^. errorAlertVis)
      || (model ^. writeSuccessVis)
      || (model ^. openErrorVis)
      || (model ^. configVis)
      || (model ^. sfmVis)
      || (model ^. csvVis)
      || (model ^. saveVis)
      || (model ^. copyVis)
      || (model ^. delCfmVis))
      
    handleFile1 :: Maybe [Text] -> AppEvent
    handleFile1 Nothing = AppNull
    handleFile1 (Just []) = AppNull
    handleFile1 (Just (f:_fs)) = AppSetInput f
    handleFile2 :: Maybe Text -> AppEvent
    handleFile2 Nothing = AppNull
    handleFile2 (Just fnm) = AppSetOutput fnm
    convertText :: InputOrth -> OutputOrth -> Text -> Text
    convertText inpO outO inpTxt = decodeKwakwalaD kcm outO $ parseKwakwalaD kcm inpO inpTxt
      where kcm = model ^. kwakConfig
    {-
    getConversion :: Text -> Text
    getConversion inpTxt = let 
      inpO = model ^. inputOrth
      outO = model ^. outputOrth
      kcm  = model ^. kwakConfig
      in decodeKwakwalaD kcm outO $ parseKwakwalaD kcm inpO inpTxt
    -}
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
      appWindowTitle "Kwak'wala Orthography Conversion (CSV)",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Monotype" "./assets/fonts/LiberationMono-Regular.ttf",
      appFontDef "Universal" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "Georgian" "./assets/fonts/NotoSansGeorgian-Regular.ttf",
      appFontDef "Umista" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "NAPA" "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "Boas" "./assets/fonts/KurintoSans-Rg.ttf",
      appFontDef "IPA"  "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "Island" "./assets/fonts/island.ttf",
      appInitEvent AppInit
      ]
    -- model = AppModel IUmista OUmista "" "" "" "" "" False False False False False "" def cfgFile
    -- Not using Ini in Model version:
    model' cfgFile (Left txt)   = AppModel {-IUmista OUmista-} "" "" IM.empty "" True False True  False False False False False False False True False 1 0 "" txt def def cfgFile Nothing
    model' cfgFile (Right iniX) = AppModel {-IUmista OUmista-} "" "" IM.empty "" True False False False False False False False False False True False 1 0 "" "" (getIniValue iniX) def cfgFile Nothing
    -- Using Ini in Model version:
    -- defIni = ini def configSpec
    -- model' cfgFile (Left txt) = AppModel IUmista OUmista "" "" "" "" "" False True False False False txt defIni cfgFile Nothing
    -- model' cfgFile (Right iniX) = AppModel IUmista OUmista "" "" "" "" "" False False False False False "" iniX cfgFile Nothing

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

readCSVMaybe :: Bool -> Maybe Char -> FilePath -> IO ([CSVError], [(Maybe Text, Bool, InputOrth, OutputOrth, Text, Text)])
readCSVMaybe rdHdrs mc fp = do
  bs <- BL.readFile fp
  csvRslt <- return $ case mc of
    Nothing  -> parseCSV bs
    (Just c) -> parseDSV False c bs
  let csvErrs = csvErrors    csvRslt
      csvTabl = csvTableFull csvRslt

      csvList = map (map getContent) csvTabl
  case csvList of
    [] -> return (csvErrs, [])
    (r:rs) -> let
      hdrs    = if rdHdrs then (map Just r) else (map (const Nothing) r)
      rs'     = if rdHdrs then rs else (r:rs)
      cols    = transpose rs'
      colsX   = map T.unlines cols
      outs    = zipWithL hdrs colsX $ \hdr col ->
                  (hdr, False, IUmista, OUmista, col, col)
      in return (csvErrs, outs)
  where
    decodeAlt :: BS.ByteString -> Text
    decodeAlt bs = case (T.decodeUtf8' bs) of
      Left _    -> "<error>"
      Right txt -> txt
    getContent :: CSVField -> Text
    -- can handle how quoting works here.
    getContent (CSVField {csvFieldContent = bs, csvFieldQuoted = bl}) = decodeAlt $ BL.toStrict bs
    getContent (CSVFieldError {}) = ""

writeFileTask :: FilePath -> FilePath -> Bool -> Char -> IM.IntMap (Maybe Text, Bool, InputOrth, OutputOrth, Text, Text) -> AppEventResponse AppModel AppEvent
writeFileTask inp fp hdrs spr mps
  | (inp == "") = Event (AppWriteError $ "No input file selected yet. Choose an input file first.")
  | (inp == fp) = Event (AppWriteError $ "Can't overwrite input file; choose a different name for output file.")
  | (fp == "" ) = Event (AppWriteError $ "No output file selected; click \"Choose Destination\" to select an output file.")
  | (fp == ".") = Event (AppWriteError $ "No output file selected; click \"Choose Destination\" to select an output file.")
  | otherwise = Task $ do
      bl <- doesFileExist fp
      if bl
        then return AppWriteExists
        else do 
          eEvt <- try @SomeException (TU.writeFile fp donList)
          case eEvt of
            Left x   -> return (AppWriteError $ T.pack (show x))
            Right () -> return AppWriteSuccess
  where
    renderError :: Text -> Text
    renderError err = "Error Trying to Save File:\n " <> err
    -- Assume that output text is always correct.
    dataList :: [(Maybe Text, Bool, InputOrth, OutputOrth, Text, Text)]
    dataList = map snd $ IM.toList mps
    newList :: [[Text]]
    newList = forL dataList $ \(hdr,_,_,_,_,txt) ->
      if hdrs
        then ((fromMaybe "" hdr):(T.lines txt))
        else (T.lines txt)
    
    altList :: [[Text]]
    altList = transpose newList

    finList :: [Text]
    finList = map (T.intercalate (T.singleton spr)) altList
    
    donList :: Text
    donList = (T.intercalate "\n" finList) <> "\n"

writeConfigTask :: FilePath -> KwakConfigModel -> IO AppEvent
writeConfigTask fp kcm = do
  rslt <- updateConfigFile' fp kcm
  case rslt of
    Just err -> return $ AppWriteError ("Error writing config file:\n" <> err)
    Nothing  -> return AppNull

nothifyText :: Text -> Maybe Text
nothifyText txt
  | T.null txt = Nothing
  | otherwise  = Just txt

{-
data AppModel = AppModel 
  { _inputFile  :: Text
  , _outputFile :: Text
  , _inputText  :: IM.IntMap (Maybe Text, Bool, InputOrth, OutputOrth, Text, Text)
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
  , _csvSep :: Maybe Char
  } deriving (Eq, Show)

-}


