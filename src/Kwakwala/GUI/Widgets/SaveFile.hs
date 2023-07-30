{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Kwakwala.GUI.Widgets.SaveFile
  ( selectSaveFile
  , selectSaveFileH
  , SFEvent(..)
  , SaveFileModel
  ) where

-- Allows manually entering in an output file name,
-- in case the dialog fails.

import Control.Lens
import Control.Lens.Lens (ALens')
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T

import Monomer

import System.Directory
import System.FilePath

import Kwakwala.GUI.Hidden.Internal

data SaveFileModel = SaveFileModel
  { _sfmWorkVal :: Text
  , _sfmFailure :: Text
  , _sfmInputFp :: Text
  } deriving (Show, Eq)

makeLenses 'SaveFileModel

instance Default SaveFileModel where
  def = (SaveFileModel "" "" "")

data SaveFileEvent 
  = RecvInputFile Text
  | SfeDone
  | SfeChecked Text
  | SfeFail Text
  | SfeCancel
  deriving (Show, Eq)

-- To be received by parent
data SFEvent
  = SFCancel
  | SFFilePath Text

-- | Using `HiddenVal`s.
selectSaveFileH :: 
  (WidgetEvent ep, CompParentModel sp)
  => ALens' sp (HiddenVal SaveFileModel) -- * Lens to the Work Value
  -> ALens' sp Text                      -- * Path of the input file
  -> (SFEvent -> ep)           -- * Function to Convert `SaveFileEvent` to app event
  -> WidgetNode sp ep
selectSaveFileH mlens flens f
  = composite_
      "Save File Select"
      ((cloneLens mlens) . hiddenVal)
      (saveFileBuilder)
      (saveFileHandler f)
      [compositeMergeModel (saveFileMergeHandler flens)]

selectSaveFile :: 
  (WidgetEvent ep, CompParentModel sp)
  => ALens' sp (SaveFileModel) -- * Lens to the Work Value
  -> ALens' sp Text            -- * Path of the input file
  -> (SFEvent -> ep)           -- * Function to Convert `SaveFileEvent` to app event
  -> WidgetNode sp ep
selectSaveFile mlens flens f
  = composite_
      "Save File Select"
      mlens
      (saveFileBuilder)
      (saveFileHandler f)
      [compositeMergeModel (saveFileMergeHandler flens)]

saveFileBuilder
  :: WidgetEnv SaveFileModel SaveFileEvent
  -> SaveFileModel
  -> WidgetNode SaveFileModel SaveFileEvent
saveFileBuilder wenv mdl = widgetTree where
  widgetTree = vstack
    [ label "Manual Output Entry" `styleBasic` [textSize 20, textCenter]
    , spacer
    , (textField_ sfmWorkVal [])
    , spacer
    , hstack
      [ label "Error"
      , spacer
      , (textField_ sfmFailure [readOnly])
      ]
    , spacer
    , button "Copy Input Directory" (RecvInputFile (mdl ^. sfmInputFp))
    , spacer
    , button "Done" SfeDone
    , spacer
    , button "Cancel" SfeCancel
    ]

saveFileHandler
  :: (WidgetEvent ep)
  => (SFEvent -> ep) 
  -> WidgetEnv SaveFileModel SaveFileEvent
  -> WidgetNode SaveFileModel SaveFileEvent
  -> SaveFileModel
  -> SaveFileEvent
  -> [EventResponse SaveFileModel SaveFileEvent sp ep]
saveFileHandler fe wenv node model evt = case evt of
  RecvInputFile txt -> [Model (model & sfmWorkVal .~ (T.pack $ takeDirectory (T.unpack txt)))]
  SfeCancel      -> [Report (fe SFCancel)]
  SfeDone        -> [Task $ checkDir' (model ^. sfmWorkVal)]
  SfeFail txt    -> [Model (model & sfmFailure .~ txt)]
  SfeChecked txt -> [Report (fe (SFFilePath txt)), Model (model & sfmFailure .~ "")]

checkDir' :: Text -> IO SaveFileEvent
checkDir' txt = either SfeFail SfeChecked <$> checkDir txt

checkDir :: Text -> IO (Either Text Text)
checkDir txt = do
  let flp = T.unpack txt
      dir = takeDirectory $ T.unpack txt
      blz = T.null txt
  bl0 <- doesFileExist flp
  bl1 <- doesDirectoryExist dir
  blx <- doesDirectoryExist flp
  if | blz -> return (Left "No Path Specified.")
     | bl0 -> return (Right txt)
     | blx -> return (Left "Output file is already an existing directory.")
     | bl1 -> return (Right txt)
     | otherwise -> return $ Left "Output directory does not exist."

saveFileMergeHandler :: CompParentModel sp
  => ALens' sp Text
  -> WidgetEnv SaveFileModel SaveFileEvent
  -> sp
  -> SaveFileModel
  -> SaveFileModel
  -> SaveFileModel
saveFileMergeHandler flens wenv pm _old model
  = model & sfmInputFp .~ (pm ^. (cloneLens flens))

