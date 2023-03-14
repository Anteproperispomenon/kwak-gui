{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Kwakwala.GUI.Config
  ( KwakConfigModel(..)
  -- * Lenses
  , kcmGrubbUseJ
  , kcmGrubbUse'
  , kcmIpaTies
  , kcmGeorgianCfg
  -- ** uh...
  , gocUseLabSign
  , gocUsePalSign
  -- * Widgets
  , kwakConfigWidget
  , kwakConfigWidgetX
  ) where

import Control.Lens
import Control.Lens.Lens (ALens')
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T

import Kwakwala.Output

import Monomer

-- | A model for the types of config
-- that are used.
data KwakConfigModel = KwakConfigModel
  { _kcmGrubbUseJ :: Bool
  , _kcmGrubbUse' :: Bool -- Keep glottal stops at word start.
  , _kcmIpaTies :: Bool
  , _kcmGeorgianCfg :: GeorgianOutputConfig
  } deriving (Eq, Show)

instance Default KwakConfigModel where
  def = KwakConfigModel
    { _kcmGrubbUseJ = True
    , _kcmGrubbUse' = False
    , _kcmIpaTies = True
    , _kcmGeorgianCfg = (GeorgianOutputConfig False False)
    }

data KwakConfigEvent
  = KwakEventTemp
  deriving (Show, Eq)

makeLenses 'KwakConfigModel

makeLenses 'GeorgianOutputConfig

-- | Create a node for a config widget
kwakConfigWidget :: WidgetEvent e => ALens' s KwakConfigModel -> (KwakConfigEvent -> e) -> WidgetNode s e
kwakConfigWidget mdlLens f = vstack $
   [ label "Orthography Settings" `styleBasic` [textSize 24, textCenter]
   , spacer
   , vscroll $ vstack
       [ label "Grubb" `styleBasic` [textSize 20, textCenter]
       , spacer
       , labeledCheckbox_ "Use 'J' to represent the phoneme /h/" ((cloneLens mdlLens) . kcmGrubbUseJ) [textRight]
       , labeledCheckbox_ "Include glottal stops before vowels at the start of a word" ((cloneLens mdlLens) . kcmGrubbUse') [textRight]
       , spacer
       , label "IPA" `styleBasic` [textSize 20, textCenter]
       , spacer
       , labeledCheckbox_ "Use ties for affricates" ((cloneLens mdlLens) . kcmIpaTies) [textRight]
       , spacer
       , label "Georgian" `styleBasic` [textSize 20, textCenter]
       , spacer
       , labeledCheckbox_ "Use Abkhaz labialisation mark" ((cloneLens mdlLens) . kcmGeorgianCfg . gocUseLabSign) [textRight]
       , labeledCheckbox_ "Use Abkhaz hard mark to indicate palatalisation of velara consonants" ((cloneLens mdlLens) . kcmGeorgianCfg . gocUsePalSign) [textRight]
       ]
   ] 

-- | Same as `kwakConfigWidget`, but doesn't
-- raise any events on its own.
kwakConfigWidgetX :: WidgetEvent e => ALens' s KwakConfigModel -> WidgetNode s e
kwakConfigWidgetX mdlLens = vstack $
   [ label "Orthography Settings" `styleBasic` [textSize 24, textCenter]
   , spacer
   , vscroll $ vstack
       [ label "Grubb" `styleBasic` [textSize 20, textCenter]
       , spacer
       , labeledCheckbox_ "Use 'J' to represent the phoneme /h/" ((cloneLens mdlLens) . kcmGrubbUseJ) [textRight]
       , labeledCheckbox_ "Include glottal stops before vowels at the start of a word" ((cloneLens mdlLens) . kcmGrubbUse') [textRight]
       , spacer
       , label "IPA" `styleBasic` [textSize 20, textCenter]
       , spacer
       , labeledCheckbox_ "Use ties for affricates" ((cloneLens mdlLens) . kcmIpaTies) [textRight]
       , spacer
       , label "Georgian" `styleBasic` [textSize 20, textCenter]
       , spacer
       , labeledCheckbox_ "Use Abkhaz labialisation mark" ((cloneLens mdlLens) . kcmGeorgianCfg . gocUseLabSign) [textRight]
       , labeledCheckbox_ "Use Abkhaz hard mark to indicate palatalisation of velara consonants" ((cloneLens mdlLens) . kcmGeorgianCfg . gocUsePalSign) [textRight]
       ]
   ] 

{-
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
-}
