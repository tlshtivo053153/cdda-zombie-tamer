module Define.Aeson
  ( cddaOption
  ) where

import Data.Aeson
import Data.Aeson.Casing

cddaCase :: String -> String
cddaCase str =
  case str of
    "CopyFrom" -> "copy-from"
    "CddaType" -> "type"
    _ -> snakeCase str

cddaOption :: Options
cddaOption = (aesonPrefix cddaCase) { omitNothingFields = True }

