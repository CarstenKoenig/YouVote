
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Poll.Models
    ( PollId, ChoiceId, Question, Choice
    , PollDescription (..)
    , PollVote (..)
    , PollStat (..)
    , Stat (..)
    , CreatePoll (..)
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import           Elm (ElmPrimitive(EInt), HasElmComparable(..))
import           GHC.Generics
import           Servant.Elm (ElmType)

import           Poll.Algebra (PollId, ChoiceId, Question, Choice)

data PollDescription = PollDescription
  { pdId       :: PollId
  , pdQuestion :: Question
  } deriving (Eq, Show, Generic)


data PollVote = PollVote
  { pvId       :: PollId
  , pvQuestion :: Question
  , pvChoices  :: Map ChoiceId Choice
  } deriving (Eq, Show, Generic)


data PollStat = PollStat
  { psId       :: PollId
  , psQuestion :: Question
  , psVotes    :: [Stat]
  } deriving (Eq, Show, Generic)


data Stat = Stat
  { vChoice :: Choice
  , vVotes  :: Int
  } deriving (Eq, Show, Generic)


data CreatePoll = CreatePoll
  { newQuestion :: Question
  , newChoices  :: [Choice]
  } deriving (Eq, Show, Generic)


$(deriveJSON defaultOptions ''PollDescription)

$(deriveJSON defaultOptions ''PollVote)

$(deriveJSON defaultOptions ''Stat)
$(deriveJSON defaultOptions ''PollStat)

$(deriveJSON defaultOptions ''CreatePoll)


instance HasElmComparable Int64 where
  toElmComparable _ = EInt

instance ElmType PollDescription
instance ElmType PollVote
instance ElmType PollStat
instance ElmType Stat
instance ElmType CreatePoll
