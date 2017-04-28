{-# LANGUAGE GADTs #-}
module Language.ASTMonad.Json
  ( module Language.ASTMonad
  , JsonEnvironment(..)
  , JsonStatement(..)
  , JsonM(..)
  , Json(..)
  , is
  , isNum
  , isBool
  , isArray
  ) where

import Language.ASTMonad
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB

data JsonEnvironment where
  JsonEnvironment :: JsonEnvironment

data JsonStatement where
  JsonString :: TB.Builder -> TB.Builder -> JsonStatement
  JsonNumber :: (Integral a) => TB.Builder -> a -> JsonStatement
  JsonBool   :: TB.Builder -> Bool -> JsonStatement
  JsonArray  :: TB.Builder -> JsonStatement

type JsonM p a = ASTM p JsonStatement JsonEnvironment a
type Json p = JsonM p ()

is :: TB.Builder -> TB.Builder -> Json p
is x y = fromCode $ \p e -> (e, JsonString x y)

isNum :: (Integral a) => TB.Builder -> a -> Json p
isNum x y = fromCode $ \p e -> (e, JsonNumber x y)

isBool :: TB.Builder -> Bool -> Json p
isBool x y = fromCode $ \p e -> (e, JsonBool x y)

isArray :: TB.Builder -> Json p -> Json p
isArray x y = fromCode' y $ \p e -> (e, JsonArray  x)
