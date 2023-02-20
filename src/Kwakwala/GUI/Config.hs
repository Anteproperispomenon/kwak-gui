{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Kwakwala.GUI.Config
  ( KwakConfigModel(..)
  , kcmGrubbUseJ
  , kcmGrubbUse'
  , kcmIpaTies
  , kwakConfigWidget
  , kwakConfigWidgetX
  ) where

import Control.Lens
import Control.Lens.Lens (ALens')
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T

import Monomer

-- | A model for the types of config
-- that are used.
data KwakConfigModel = KwakConfigModel
  { _kcmGrubbUseJ :: Bool
  , _kcmGrubbUse' :: Bool -- Keep glottal stops at word start.
  , _kcmIpaTies :: Bool
  } deriving (Eq, Show)

instance Default KwakConfigModel where
  def = KwakConfigModel
    { _kcmGrubbUseJ = True
    , _kcmGrubbUse' = False
    , _kcmIpaTies = True
    }

data KwakConfigEvent
  = KwakEventTemp
  deriving (Show, Eq)

makeLenses 'KwakConfigModel

-- | Create a node for a config widget
kwakConfigWidget :: WidgetEvent e => ALens' s KwakConfigModel -> (KwakConfigEvent -> e) -> WidgetNode s e
kwakConfigWidget mdlLens f = vscroll $ vstack
  [ label "Grubb" `styleBasic` [textSize 20, textCenter]
  , labeledCheckbox "Use 'J' to represent the phoneme /h/" ((cloneLens mdlLens) . kcmGrubbUseJ)
  , labeledCheckbox "Include glottal stops before vowels at the start of a word" ((cloneLens mdlLens) . kcmGrubbUse')
  , spacer
  , label "IPA" `styleBasic` [textSize 20, textCenter]
  , labeledCheckbox "Use ties for affricates" ((cloneLens mdlLens) . kcmIpaTies)
  ]

-- | Same as `kwakConfigWidget`, but doesn't
-- raise any events on its own.
kwakConfigWidgetX :: WidgetEvent e => ALens' s KwakConfigModel -> WidgetNode s e
kwakConfigWidgetX mdlLens = vscroll $ vstack
  [ label "Orthography Settings" `styleBasic` [textSize 24, textCenter]
  , spacer
  , label "Grubb" `styleBasic` [textSize 20, textCenter]
  , labeledCheckbox "Use 'J' to represent the phoneme /h/" ((cloneLens mdlLens) . kcmGrubbUseJ)
  , labeledCheckbox "Include glottal stops before vowels at the start of a word" ((cloneLens mdlLens) . kcmGrubbUse')
  , spacer
  , label "IPA" `styleBasic` [textSize 20, textCenter]
  , labeledCheckbox "Use ties for affricates" ((cloneLens mdlLens) . kcmIpaTies)
  ]


