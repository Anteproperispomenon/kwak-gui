{-|
Module      : Kwakwala.GUI.Info
Description : Pre-Written Tooltip Text
Copyright   : (c) David Wilson, 2023
License     : BSD-3

Explanations to be used for mouseover text
in the main GUI.

-}

module Kwakwala.GUI.Info
  ( tooltipK
  , ttUmista
  , ttNapa
  , ttGrubb
  , ttGeorgian
  , ttIpa
  , ttBoas
  , ttBoas'
  , ttIsland
  ) where

import Data.Text qualified as T
import Data.Text (Text)

import Monomer.Core.WidgetTypes
import Monomer.Widgets.Containers.Tooltip

tooltipK :: Text -> WidgetNode s e -> WidgetNode s e
tooltipK txt = tooltip_ txt [tooltipDelay 600]

ttGrubb :: Text
ttGrubb = "ASCII-compatible orthography based on David Grubb's orthography."

ttNapa :: Text
ttNapa = "Orthography based on NAPA used around Campbell River. Sometimes referred to as IPA."

ttUmista :: Text
ttUmista = "Orthography primarily used in Northern Vancouver Island and surrounding islands."

ttGeorgian :: Text
ttGeorgian = "Experimental orthography based on Mkhedruli script used by Georgian and other Kartvelian languages."

ttIpa :: Text
ttIpa = "Official International Phonetic Alphabet. Not commonly used to represent Kwak'wala."

ttBoas :: Text
ttBoas = "Input for Boas's original orthography."

ttBoas' :: Text
ttBoas' = "Output orthography based on Boas's original orthography."

ttIsland :: Text
ttIsland = "\"Orthography\" for the \"Island\" font to represent NAPA."
