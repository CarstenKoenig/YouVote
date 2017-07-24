{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy  (Proxy (Proxy))
import           Elm         (Spec (Spec), specsToDir, toElmTypeSource,
                              toElmDecoderSource, toElmEncoderSource)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                              generateElmForAPIWith, UrlPrefix (Dynamic))

import           Lib         (API)
import           Poll.Models (Addition)


elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Dynamic }


specs :: [Spec]
specs =
  [ Spec ["Api"]
         (defElmImports
          : toElmTypeSource    (Proxy :: Proxy Addition)
          : toElmDecoderSource (Proxy :: Proxy Addition)
          : toElmEncoderSource (Proxy :: Proxy Addition)
          : generateElmForAPIWith elmOpts  (Proxy :: Proxy API))
  ]


main :: IO ()
main = specsToDir specs "src/client"
