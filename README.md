# ast-monad-json
A library for writing JSON

## Example
```Haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.ASTMonad
import Language.ASTMonad.Json
import Language.ASTMonad.Json.Renderer
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB

data Parameter = Parameter { px :: Int, py :: Int }

jsonCode :: Json Parameter
jsonCode = do
  "id1" `is` "val1"
  "id2" `is` "val2"
  "flag" `isBool` True
  "point" `isArray` do
    x <- getParam px
    y <- getParam py
    "px" `isNum` x
    "py" `isNum` y

main :: IO ()
main = do
  let param = Parameter { px = 123, py = 456 }
  TL.putStrLn $ TB.toLazyText $ renderJson $ buildAST jsonCode param JsonEnvironment
```

The execution result of the above code is

```
{"id1":"val1","id2":"val2","flag":true,"point":{"px":123,"py":456}}
```

