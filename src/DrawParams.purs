module DrawParams where

import Prelude

create :: Number -> Number -> Settings
create width height =
  { canvasWidth : width
  , canvasHeight : height
  , blockWidth : width / 4.0
  , blockHeight : width / 4.0
  }

type Settings =
  { canvasWidth :: Number
  , canvasHeight :: Number
  , blockWidth :: Number
  , blockHeight :: Number
  }