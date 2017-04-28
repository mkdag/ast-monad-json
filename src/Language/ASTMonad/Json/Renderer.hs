{-# LANGUAGE OverloadedStrings #-}
module Language.ASTMonad.Json.Renderer
  ( renderJson
  ) where

import Language.ASTMonad
import Language.ASTMonad.Json
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Monoid((<>))
import Data.Foldable(fold)
import Data.List(intersperse)

renderJson :: (JsonEnvironment, CodeSeq JsonStatement) -> TB.Builder
renderJson (e, cs) = if null cs'
                      then ""
                      else "{" <> ss <> "}"
  where cs' = fromCodeSeq cs
        ss  = fold $ intersperse "," $ map f cs'
        f (Code s cs') = beginning <> renderJson (e, cs') <> ending
          where (beginning, ending) = renderStatement e s

renderStatement :: JsonEnvironment -> JsonStatement -> (TB.Builder, TB.Builder)
renderStatement _ (JsonString x y) = ("\"" <> x <> "\"" <> ":" <> "\"" <> y <> "\"", "")
renderStatement _ (JsonNumber x y) = ("\"" <> x <> "\"" <> ":" <> TB.decimal y, "")
renderStatement _ (JsonBool x y) = ("\"" <> x <> "\"" <> ":" <> (if y then "true" else "false"), "")
renderStatement _ (JsonArray x) = ("\"" <> x <> "\"" <> ":" <> "", "")
