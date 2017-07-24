
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Poll.Models
    ( Addition (..)
    , Poll (..)
    , PollChoice (..)
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text (Text)
import           GHC.Generics
import           Servant.Elm (ElmType)


data Addition = Addition
  { operandA      :: Int
  , operandB      :: Int
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Addition)


data Poll = Poll
  { question :: Text
  , choices  :: [PollChoice]
  } deriving (Eq, Show, Generic)


data PollChoice = PollChoice
  { answer :: Text
  , votes  :: Maybe Int
  } deriving (Eq, Show, Generic)


$(deriveJSON defaultOptions ''PollChoice)
$(deriveJSON defaultOptions ''Poll)

instance ElmType Addition
instance ElmType PollChoice
instance ElmType Poll
