
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Poll.Models
    ( Addition (..)
    , Poll (..)
    , PollChoice (..)
    , CreatePoll (..)
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics
import           Servant.Elm (ElmType)


data Addition = Addition
  { operandA      :: Int
  , operandB      :: Int
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Addition)


data Poll = Poll
  { pollId   :: Int64
  , question :: Text
  , choices  :: [PollChoice]
  } deriving (Eq, Show, Generic)


data PollChoice = PollChoice
  { choiceId :: Int64
  , answer :: Text
  , votes  :: Maybe Int
  } deriving (Eq, Show, Generic)


data CreatePoll = CreatePoll
  { newQuestion :: Text
  , newChoices  :: [Text]
  } deriving (Eq, Show, Generic)



$(deriveJSON defaultOptions ''PollChoice)
$(deriveJSON defaultOptions ''Poll)
$(deriveJSON defaultOptions ''CreatePoll)


instance ElmType Addition
instance ElmType PollChoice
instance ElmType Poll
instance ElmType CreatePoll
