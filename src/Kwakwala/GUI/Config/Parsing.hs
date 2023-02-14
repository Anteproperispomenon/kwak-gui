module Kwakwala.GUI.Config.Parsing 
  ( configSpec
  , parseConfig
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


parseConfig :: T.Text -> Either T.Text (Ini KwakConfigModel)
parseConfig txt = case (parseIni txt (ini def configSpec)) of
  Left  str -> Left  $ T.pack str
  Right ins -> Right $ ins