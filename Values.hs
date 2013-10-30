module Values  where

import ObjCSyntax

data Value = Value {
  className :: String,
  fields :: [Field]
} deriving (Eq,Show)

data Field = Field { typ :: Type, name :: String }
 deriving (Eq,Show)
