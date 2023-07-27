{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

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
  , _errorMsg :: Text
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
          , hstack $
             [ label "Separator"
             , optionButton "," Nothing csvSep 
             , spacer
             , optionButton ";" (Just ';') csvSep
             , spacer
             , optionButton "Tab" (Just '\t') csvSep
             ]
          , spacer
          , labeledCheckbox "Get Headers from File" readHeaders
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
          , hstack $
             [ label "Separator"
             , optionButton "," Nothing csvSep 
             , spacer
             , optionButton ";" (Just ';') csvSep
             , spacer
             , optionButton "Tab" (Just '\t') csvSep
             ]
          , spacer
          , labeledCheckbox "Save Headers to File" readHeaders
          , spacer
          , button "Save File" AppWriteFile
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
    , spreadColumns (model ^. inputText)
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
  -- okay
  spreadColumns :: IM.IntMap (Maybe Text, Bool, InputOrth, OutputOrth, Text, Text) -> WidgetNode AppModel AppEvent
  spreadColumns strs = hgrid $ forWithKey strs $ \key (hdr, cvtble, iorth, oorth, itxt, otxt) ->
    vstack $
      [ label $ fromMaybe ("Column " <> showt key) hdr
      , labeledCheckboxV "Modify?" cvtble (\bl -> AppChangeModify key bl)
      , label "Input"
      -- , textDropdown (inputText . at key . _4) [IUmista, INapa, IGrubb, IGeorgian, IBoas, IIsland]
      , textDropdownV iorth (\io -> AppChangeIOrth key io) [IUmista, INapa, IGrubb, IGeorgian, IBoas, IIsland]
      , label "Output"
      -- , textDropdown (inputText . at key . _5) [OUmista, ONapa, OGrubb, OGeorgian, OBoas, OIsland, OIpa]
      , textDropdownV oorth (\oo -> AppChangeOOrth key oo) [OUmista, ONapa, OGrubb, OGeorgian, OBoas, OIsland, OIpa]
      , textAreaV_ otxt (\_ -> AppNull) [readOnly] 
          `styleBasic` [textFont $ selectFontO $ oorth]
      ]
  
  {-
  spreadColumns :: [String] -> [Text] -> WidgetNode AppModel AppEvent
  spreadColumns hdrs txts
    = hgrid $ zipWith3L hdrs [0..]  $ \hdr txt n ->
        vstack $
          [ optionButton $ T.pack hdr
          , textAreaV_ txt (\_ -> AppNull) [readOnly]
          ]
  -}

forWithKey :: IM.IntMap a -> (Int -> a -> b) -> IM.IntMap b
forWithKey = flip IM.mapWithKey

forL :: [a] -> (a -> b) -> [b]
forL xs f = map f xs

zipWithL :: [a] -> [b] -> (a -> b -> c) -> [c]
zipWithL l1 l2 f = zipWith f l1 l2

zipWith3L :: [a] -> [b] -> [c] -> (a -> b -> c -> d) -> [d]
zipWith3L l1 l2 l3 f = zipWith3 f l1 l2 l3

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
  -- AppIncrease -> [Model (model & clickCount +~ 1)]
  (AppSetInput  fnm) -> [Model (model & inputFile  .~ fnm & csvVis .~ True)]
  (AppSetInput2    ) -> let fnm = (model ^. inputFile) in [(Task $ AppGotInput <$> readCSVMaybe (model ^. csvSep) (T.unpack fnm)), (Model (model & csvVis .~ False))]
  (AppSetOutput fnm) -> [Model (model & outputFile .~ fnm & sfmVis .~ False & saveVis .~ True)]
  -- (AppChangeIOrth ky io) -> [Model (model & inputText . ix ky . _3 .~ io)]
  (AppChangeIOrth ky io) 
    -> [Model 
         (model & inputText . at ky %~ \case
           Nothing -> Nothing
           Just (hdr, bl, oldIo, oldOo, itxt, otxt) -> Just (hdr, bl, io, oldOo, itxt, convertText io oldOo itxt)
         )
       ]
  -- (AppChangeOOrth ky oo) -> [Model (model & inputText . ix ky . _4 .~ oo)]
  (AppChangeOOrth ky oo) 
    -> [Model 
         (model & inputText . at ky %~ \case
           Nothing -> Nothing
           Just (hdr, bl, oldIo, oldOo, itxt, otxt) -> Just (hdr, bl, oldIo, oo, itxt, convertText oldIo oo itxt)
         )
       ]
  -- (AppChangeModify ky b) -> [Model (model & inputText . ix ky . _2 .~ b )]
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

  AppOpenFile -> [Task $ handleFile1 <$> openFileDialog "Open Input File" "" ["*.txt", "*.*"] "Text Files" False]
  AppGotInput (errs, cols) -> 
    let model' = (model & inputText .~ (IM.fromList (zip [1..] cols)))
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
  AppSaveFile -> [Task $ handleFile2 <$> saveFileDialog "Select Output File" (model ^. inputFile) ["*.csv", "*.*"] "Text Files"]
  {-
  AppSaveFileMan  -> [Model (model & sfmVis .~ True)]
  AppOpenFileKey  -> if checkNoPopups then [Event AppOpenFile]  else []
  AppSaveFileKey  -> if checkNoPopups then [Event AppSaveFile]  else []
  AppWriteFileKey -> if checkNoPopups then [Event AppWriteFile] else []
  AppGotInput mtxt -> case mtxt of
     (Just txt) -> [Model (model & inputText .~ txt & outputText .~ getConversion txt)]
     Nothing    -> [Model (model & openErrorVis .~ True)]
  -- Technically not a task anymore; it's just a bit too complicated to fit here.
  -- _ -> []
  AppWriteFile -> [writeFileTask (T.unpack (model ^. inputFile)) (T.unpack (model ^. outputFile)) (model ^. outputText)]
  AppWriteSuccess -> [Model (model & writeSuccessVis .~ True)] -- Display a pop-up message, maybe?
  AppWriteExists -> [Model (model & overwriteConfVis .~ True)]
  (AppWriteError err) -> [Model (model & errorMsg .~ (renderError err) & errorAlertVis .~ True), tlogErrTask err]
  AppOverWrite -> [Task $ overWriteFileTask (T.unpack (model ^. outputFile)) (model ^. outputText), Model (model & overwriteConfVis .~ False)] 
  AppRefresh  -> [Model (model & outputText .~ getConversion (model ^. inputText))]
  AppRefreshI -> [Model (model & outputText .~ getConversion (model ^. inputText) & inputText %~ modText)]
  (AppCurDir fp) -> [Model (model & currentDir .~ (T.pack fp))]
  AppDoneConfig -> [Event AppRefresh, Model (model & configVis .~ False), Task $ writeConfigTask (model ^. cfgFilePath) (model ^. kwakConfig)]
  -- AppDoneConfig -> let newCfg = selfUpdate (model ^. kwakConfig)
  --   in [Model (model & configVis .~ False & kwakConfig .~ newCfg)] -- Add task here to update config file.
  AppOpenConfig -> [Model (model & configVis .~ True )]
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
      || (model ^. saveVis))
      
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
      appFontDef "IPA"  "./assets/fonts/DoulosSIL-Regular.ttf",
      appFontDef "Island" "./assets/fonts/island.ttf",
      appInitEvent AppInit
      ]
    -- model = AppModel IUmista OUmista "" "" "" "" "" False False False False False "" def cfgFile
    -- Not using Ini in Model version:
    model' cfgFile (Left txt)   = AppModel {-IUmista OUmista-} "" "" IM.empty "" True False True False False False False False False txt def def cfgFile Nothing
    model' cfgFile (Right iniX) = AppModel {-IUmista OUmista-} "" "" IM.empty "" True False False False False False False False False "" (getIniValue iniX) def cfgFile Nothing
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

readCSVMaybe :: Maybe Char -> FilePath -> IO ([CSVError], [(Maybe Text, Bool, InputOrth, OutputOrth, Text, Text)])
readCSVMaybe mc fp = do
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
      hdrs    = map Just r
      cols    = transpose rs
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


