module Kwakwala.GUI.Types 
  ( InputOrth(..)
  , OutputOrth(..)
  , parseKwakwalaD
  , decodeKwakwalaD
  , orthI2O
  , orthO2I
  ) where

import Data.Text (Text)
import TextShow

import Kwakwala.GUI.Config (KwakConfigModel(..))
import Kwakwala.Parsers
import Kwakwala.Output
import Kwakwala.Sounds

-- | Inpot Orthography Option
data InputOrth
  = IUmista
  | INapa
  | IGrubb
  | IGeorgian
  | IBoas
  deriving (Show, Eq)

instance TextShow InputOrth where
  showb IUmista = "IUmista"
  showb INapa = "INapa"
  showb IGrubb = "IGrubb"
  showb IGeorgian = "IGeorgian"
  showb IBoas = "IBoas"

-- | Output Orthography Option.
data OutputOrth
  = OUmista
  | ONapa
  | OGrubb
  | OGeorgian
  | OBoas
  | OIpa
  deriving (Show, Eq)

instance TextShow OutputOrth where
  showb OUmista = "OUmista"
  showb ONapa = "ONapa"
  showb OGrubb = "OGrubb"
  showb OGeorgian = "OGeorgian"
  showb OBoas = "OBoas"
  showb OIpa = "OIpa"
    
parseKwakwalaD :: KwakConfigModel -> InputOrth -> Text -> [CasedChar]
parseKwakwalaD _ IUmista   = encodeFromUmista
parseKwakwalaD _ INapa     = encodeFromNapa
parseKwakwalaD _ IGrubb    = encodeFromGrubbAscii
parseKwakwalaD _ IBoas     = encodeFromBoas
parseKwakwalaD _ IGeorgian = encodeFromGeorgian

decodeKwakwalaD :: KwakConfigModel -> OutputOrth -> [CasedChar] -> Text
decodeKwakwalaD _ OUmista   = decodeToUmista
decodeKwakwalaD _ ONapa     = decodeToNapa
decodeKwakwalaD _ OBoas     = decodeToPseudoBoas
decodeKwakwalaD _ OIpa      = decodeToIpa
decodeKwakwalaD _ OGeorgian = decodeToGeorgianTitle
decodeKwakwalaD kcm OGrubb
  | (_kcmGrubbUseJ kcm) = decodeToGrubbAsciiJ
  | otherwise           = decodeToGrubbAscii

orthI2O :: InputOrth -> OutputOrth
orthI2O IUmista = OUmista
orthI2O INapa = ONapa
orthI2O IGrubb = OGrubb
orthI2O IGeorgian = OGeorgian
orthI2O IBoas = OBoas

orthO2I :: OutputOrth -> Maybe InputOrth
orthO2I OUmista = Just IUmista
orthO2I ONapa = Just INapa
orthO2I OGrubb = Just IGrubb
orthO2I OGeorgian = Just IGeorgian
orthO2I OBoas = Just IBoas
orthO2I _ = Nothing

