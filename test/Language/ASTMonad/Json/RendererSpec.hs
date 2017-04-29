{-# LANGUAGE OverloadedStrings #-}
module Language.ASTMonad.Json.RendererSpec
  ( spec
  ) where

import Test.Hspec
import Language.ASTMonad.Json
import Language.ASTMonad.Json.Renderer
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

f :: Json() -> TL.Text
f jsonCode = TB.toLazyText $ renderJson $ buildAST jsonCode () JsonEnvironment

spec :: Spec
spec = do
  describe "Language.ASTMonad.Json.Renderer.renderJson" $ do
    context "when JsonString" $ do
      it "should return \"str1\":\"str2\"" $ do
        (f $ "id1" `is` "val1") `shouldBe` "{\"id1\":\"val1\"}"
    context "when JsonNumber" $ do
      it "should return \"str\":num" $ do
        (f $ "num1" `isNum` 123) `shouldBe` "{\"num1\":123}"
    context "when JsonBool" $ do
      it "should return \"str\":true or \"str\":false" $ do
        (f $ "flag" `isBool` True) `shouldBe` "{\"flag\":true}"
        (f $ "flag" `isBool` False) `shouldBe` "{\"flag\":false}"
    context "when JsonArray" $ do
      it "should return \"str\":{..}" $ do
        (f $ "array" `isArray` do
            "id1" `is` "val1") `shouldBe` "{\"array\":{\"id1\":\"val1\"}}"
    
