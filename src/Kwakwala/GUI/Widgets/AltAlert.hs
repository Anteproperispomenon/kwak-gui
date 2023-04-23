module Kwakwala.GUI.Widgets.AltAlert
  ( altAlertMsg'
  , altAlertMsg
  , altAlertMsg_
  , altAlert
  , altAlert_
  ) where

-- A variation on Alert that allows
-- updatable content

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (%~), (^.))
import Data.Default
import Data.Maybe

import Monomer
import Data.Text    qualified as T
import Monomer.Lens qualified as L

import Data.Text (Text)

altAlertMsg' :: (WidgetModel s, WidgetEvent e) => T.Text -> e -> WidgetNode s e
altAlertMsg' txt evt = vstack_ [sizeReqUpdater clearExtra]
  [ hstack
      [ filler
      , box_ [alignTop, onClick evt] (icon IconClose)
      ]
  , label_ txt [multiline]
  , filler
  , box_ [alignRight] (hstack [button "Close" evt])
  ] `styleBasic` [bgColor dimGray, padding 10, border 3 black, radius 7]

{-|
Configuration options for alt alert:

- 'titleCaption': the title of the alert dialog.
- 'closeCaption': the caption of the close button.
-}
data AltAlertCfg = AltAlertCfg {
  _aalcTitle :: Maybe Text,
  _aalcClose :: Maybe Text
}

instance Default AltAlertCfg where
  def = AltAlertCfg {
    _aalcTitle = Nothing,
    _aalcClose = Nothing
  }

instance Semigroup AltAlertCfg where
  (<>) a1 a2 = AltAlertCfg {
    _aalcTitle = _aalcTitle a2 <|> _aalcTitle a1,
    _aalcClose = _aalcClose a2 <|> _aalcClose a1
  }

instance Monoid AltAlertCfg where
  mempty = def

instance CmbTitleCaption AltAlertCfg where
  titleCaption t = def {
    _aalcTitle = Just t
  }

instance CmbCloseCaption AltAlertCfg where
  closeCaption t = def {
    _aalcClose = Just t
  }


-- | Creates an alert dialog with the provided content.
altAlert
  :: (Eq s, WidgetModel s, WidgetEvent e)
  => e                -- ^ The event to raise when the dialog is closed.
  -> WidgetNode s e   -- ^ The content to display in the dialog.
  -> WidgetNode s e   -- ^ The created dialog.
altAlert evt dialogBody = altAlert_ evt def dialogBody

-- | Creates an alert dialog with the provided content. Accepts config.
altAlert_
  :: (Eq s, WidgetModel s, WidgetEvent e)
  => e                -- ^ The event to raise when the dialog is closed.
  -> [AltAlertCfg]       -- ^ The config options for the dialog.
  -> WidgetNode s e   -- ^ The content to display in the dialog.
  -> WidgetNode s e   -- ^ The created dialog.
altAlert_ evt configs dialogBody = newNode where
  config = mconcat configs
  createUI = buildUI (const dialogBody) evt config
  newNode = composite_ "alt alert" (id) createUI handleEvent []

-- | Creates an alert dialog with a text message as content.
altAlertMsg
  :: (Eq s, WidgetModel s, WidgetEvent e)
  => Text            -- ^ The message to display.
  -> e               -- ^ The event to raise when the dialog is closed.
  -> WidgetNode s e  -- ^ The created dialog.
altAlertMsg message evt = altAlertMsg_ message evt def

-- | Creates an alert dialog with a text message as content. Accepts config.
altAlertMsg_
  :: (Eq s, WidgetModel s, WidgetEvent e)
  => Text            -- ^ The message to display.
  -> e               -- ^ The event to raise when the dialog is closed.
  -> [AltAlertCfg]      -- ^ The config options for the dialog.
  -> WidgetNode s e  -- ^ The created dialog.
altAlertMsg_ message evt configs = newNode where
  config = mconcat configs
  dialogBody wenv = label_ message [multiline]
    & L.info . L.style .~ collectTheme wenv L.dialogMsgBodyStyle
  createUI = buildUI dialogBody evt config
  newNode = composite_ "alt alert" (id) createUI handleEvent []

-- Trying to do it with
-- e ~ ep
buildUI
  :: (Eq s, WidgetModel s, WidgetEvent e)
  => (WidgetEnv s e -> WidgetNode s e)
  -> e
  -> AltAlertCfg
  -> WidgetEnv s e
  -> s
  -> WidgetNode s e
buildUI dialogBody cancelEvt config wenv model = mainTree where
  title = fromMaybe "" (_aalcTitle config)
  close = fromMaybe "Close" (_aalcClose config)

  emptyOverlay = collectTheme wenv L.emptyOverlayStyle
  dismissButton = hstack [button close cancelEvt]
  closeIcon = icon_ IconClose [width 2]
    & L.info . L.style .~ collectTheme wenv L.dialogCloseIconStyle

  alertTree = vstack_ [sizeReqUpdater clearExtra] [
      hstack [
        label title & L.info . L.style .~ collectTheme wenv L.dialogTitleStyle,
        filler,
        box_ [alignTop, onClick cancelEvt] closeIcon
      ],
      dialogBody wenv,
      filler,
      box_ [alignRight] dismissButton
        & L.info . L.style .~ collectTheme wenv L.dialogButtonsStyle
    ] & L.info . L.style .~ collectTheme wenv L.dialogFrameStyle
  alertBox = box_ [onClickEmpty cancelEvt] (boxShadow alertTree)
    & L.info . L.style .~ emptyOverlay
  mainTree = keystroke [("Esc", cancelEvt)] alertBox

handleEvent
  :: WidgetEnv s e
  -> WidgetNode s e
  -> s
  -> e
  -> [EventResponse s e s e]
handleEvent wenv node model evt = [Report evt]

