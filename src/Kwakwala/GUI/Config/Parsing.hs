module Kwakwala.GUI.Config.Parsing 
  ( configSpec
  , parseConfig
  , selfUpdate
  ) where

-- not specifying anything so that lenses come through.
import Kwakwala.GUI.Config

-- import Control.Lens
import Data.Default (def)
import Data.Ini.Config.Bidir

import Data.Text qualified as T

configSpec :: IniSpec KwakConfigModel ()
configSpec = do
  section "GRUBB" $ do
    kcmGrubbUseJ .= field "use-j" bool
                      & comment ["If true, use the letter 'j' to", "represent the phoneme /h/."]
    kcmGrubbUse' .= field "glottal-start" bool
                      & comment ["If false, don't notate glottal stops before", "vowels at the beginnings of words"]
    kcmGrubbUse7 .= field "use-7" bool
                      & comment ["If true, replace apostrophes with the number 7."]
  section "IPA" $ do
    kcmIpaTies   .= field "use-ties" bool
                      & comment ["If true, use ties for affricates in IPA."]
  section "GEORGIAN" $ do
    (kcmGeorgianCfg . gocUseLabSign) .= field "alt-lab" bool
                      & comment ["If true, use the Abkhaz labialisation mark", "to represent labialisation."]
    (kcmGeorgianCfg . gocUsePalSign) .= field "pal-vis" bool
                      & comment ["If true, use the Abkhaz palatalisation mark", "to explicitly mark palatalisation."]

parseConfig :: T.Text -> Either T.Text (Ini KwakConfigModel)
parseConfig txt = case (parseIni txt (ini def configSpec)) of
  Left  str -> Left  $ T.pack str
  Right ins -> Right $ ins

-- | Update using the current value. 
-- Provided since using `iniValueL`
-- doesn't perform `updateIni`.
selfUpdate :: Ini s -> Ini s
selfUpdate iniX = updateIni (getIniValue iniX) iniX

instance Eq (Ini KwakConfigModel) where
  x == y =
    (getIniValue x) == (getIniValue y)
      && (getRawIni x) == (getRawIni y)

instance Show (Ini KwakConfigModel) where
  show x = "Ini (" ++ show (getIniValue x) ++ ")"