{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy  (Proxy (Proxy))
import qualified Data.Text as Text
import           Elm         (Spec (Spec), specsToDir, toElmTypeSource,
                              toElmDecoderSource, toElmEncoderSource)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                              generateElmForAPIWith, UrlPrefix (Dynamic))

import           Application (API)
import           Poll.Models (Addition, Poll, PollChoice, CreatePoll)


elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Dynamic }


specs :: [Spec]
specs =
  [ Spec ["Api"]
    ( defElmImports `Text.append` "import Dict exposing (Dict)"
      : toElmTypeSource    (Proxy :: Proxy Addition)
      : toElmDecoderSource (Proxy :: Proxy Addition)
      : toElmEncoderSource (Proxy :: Proxy Addition)
      : toElmTypeSource     (Proxy :: Proxy PollChoice)
      : toElmDecoderSource  (Proxy :: Proxy PollChoice)
      : toElmTypeSource     (Proxy :: Proxy Poll)
      : toElmDecoderSource  (Proxy :: Proxy Poll)
      : toElmTypeSource     (Proxy :: Proxy CreatePoll)
      : toElmEncoderSource  (Proxy :: Proxy CreatePoll)
      : generateElmForAPIWith elmOpts  (Proxy :: Proxy API))
  ]


main :: IO ()
main = specsToDir specs "src/client"
