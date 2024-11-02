module Syntax where

import Data.ByteString.Char8 (ByteString)

data Ty
  = TyInt
  | TyBool
  | TyArr Ty Ty
  | TyVar Int
  deriving (Eq, Show)

data Exp
  = Var ByteString
  | Int Integer
  | Bool Bool
  | Fun [(ByteString, Maybe Ty)] Exp
  | App Exp [Exp]
  | Let ByteString (Maybe Ty) Exp Exp
  | If Exp Exp Exp
  deriving (Eq, Show)
