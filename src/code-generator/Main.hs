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
import           Poll.Models


elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Dynamic }


specs :: [Spec]
specs =
  [ Spec ["Api"]
    ( defElmImports `Text.append` "import Dict exposing (Dict)"
      : toElmTypeSource     (Proxy :: Proxy PollDescription)
      : toElmDecoderSource  (Proxy :: Proxy PollDescription)
      : toElmTypeSource     (Proxy :: Proxy PollVote)
      : toElmDecoderSource  (Proxy :: Proxy PollVote)
      : toElmTypeSource     (Proxy :: Proxy PollStat)
      : toElmDecoderSource  (Proxy :: Proxy PollStat)
      : toElmTypeSource     (Proxy :: Proxy Stat)
      : toElmDecoderSource  (Proxy :: Proxy Stat)
      : toElmTypeSource     (Proxy :: Proxy CreatePoll)
      : toElmEncoderSource  (Proxy :: Proxy CreatePoll)
      : generateElmForAPIWith elmOpts  (Proxy :: Proxy API))
  ]


main :: IO ()
main = specsToDir specs "src/client"
