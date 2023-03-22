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
  | IIsland
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
  | OIsland
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
parseKwakwalaD _ IIsland   = encodeFromIsland

decodeKwakwalaD :: KwakConfigModel -> OutputOrth -> [CasedChar] -> Text
decodeKwakwalaD _ OUmista   = decodeToUmista
decodeKwakwalaD _ ONapa     = decodeToNapa
decodeKwakwalaD _ OBoas     = decodeToPseudoBoas
decodeKwakwalaD kcm OGeorgian = decodeToGeorgianC (_kcmGeorgianCfg kcm)
decodeKwakwalaD kcm OGrubb
  | (_kcmGrubbUseJ kcm && _kcmGrubbUse' kcm) = decodeToGrubbAsciiJX
  | (_kcmGrubbUseJ kcm) = decodeToGrubbAsciiJ
  | (_kcmGrubbUse' kcm) = decodeToGrubbAsciiX
  | otherwise           = decodeToGrubbAscii
decodeKwakwalaD _ OIsland = decodeToIsland
decodeKwakwalaD kcm OIpa
  | (_kcmIpaTies kcm) = decodeToIpa
  | otherwise         = decodeToIpaAlt

orthI2O :: InputOrth -> OutputOrth
orthI2O IUmista = OUmista
orthI2O INapa = ONapa
orthI2O IGrubb = OGrubb
orthI2O IGeorgian = OGeorgian
orthI2O IBoas = OBoas
orthI2O IIsland = OIsland

orthO2I :: OutputOrth -> Maybe InputOrth
orthO2I OUmista = Just IUmista
orthO2I ONapa = Just INapa
orthO2I OGrubb = Just IGrubb
orthO2I OGeorgian = Just IGeorgian
orthO2I OBoas = Just IBoas
orthO2I OIsland = Just IIsland
orthO2I _ = Nothing

