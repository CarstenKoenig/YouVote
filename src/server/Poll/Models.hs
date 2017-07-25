
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Poll.Models
    ( Addition (..)
    , PollId, ChoiceId
    , Poll (..)
    , PollChoice (..)
    , CreatePoll (..)
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Elm (ElmPrimitive(EInt), HasElmComparable(..))
import           GHC.Generics
import           Servant.Elm (ElmType)


data Addition = Addition
  { operandA      :: Int
  , operandB      :: Int
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Addition)

type PollId = Int64
type ChoiceId = Int64

data Poll = Poll
  { pollId   :: PollId
  , question :: Text
  , choices  :: Map ChoiceId PollChoice
  } deriving (Eq, Show, Generic)


data PollChoice = PollChoice
  { choiceId :: ChoiceId
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



instance HasElmComparable Int64 where
  toElmComparable _ = EInt

  
instance ElmType Addition
instance ElmType PollChoice
instance ElmType Poll
instance ElmType CreatePoll
