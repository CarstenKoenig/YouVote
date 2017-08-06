{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
module Poll.Algebra
  ( Repository
  , RepositoryF (..)
  , InterpretRepository (..)
  , PollId, ChoiceId, Question, Choice, IpAddr
  , PollHeader (..), PollData (..), PollChoice (..)
  , recentPolls, loadPoll, getIp, userPolls, createPoll, registerVote
  ) where


import qualified Control.Monad.Free as Free
import           Control.Monad.Free (Free)

import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Text (Text)


----------------------------------------------------------------------
-- low-level api

getIp :: Repository IpAddr
getIp = Free.liftF $ GetIp id


recentPolls :: Int -> Repository [PollHeader]
recentPolls count =
  Free.liftF $ RecentPolls count id


userPolls :: IpAddr -> Repository [PollHeader]
userPolls ipAdr =
  Free.liftF $ UserPolls ipAdr id


loadPoll :: PollId -> Repository (Maybe PollData)
loadPoll pollId =
  Free.liftF $ LoadPoll pollId id


createPoll :: IpAddr -> Question -> [Choice] -> Repository PollId
createPoll ipAdr question choices =
  Free.liftF $ CreatePoll ipAdr question choices id

  
registerVote :: IpAddr -> PollId -> ChoiceId -> Repository ()
registerVote ipAdr pollId choiceId =
  Free.liftF $ RegisterVote ipAdr pollId choiceId ()


----------------------------------------------------------------------
-- type definitions

type PollId = Int64
type ChoiceId = Int64

type Question = Text
type Choice = Text


type IpAddr = String


data PollHeader = PollHeader
  { phId       :: PollId
  , phQuestion :: Question
  , phCreator  :: IpAddr
  } deriving Show


data PollData = PollData
  { pdId       :: PollId
  , pdQuestion :: Question
  , pdCreator  :: IpAddr
  , pdChoices  :: Map ChoiceId PollChoice
  } deriving Show


data PollChoice = PollChoice
  { pcPollId   :: PollId
  , pcId       :: ChoiceId
  , pcChoice   :: Choice
  , pcVotes    :: Set IpAddr
  } deriving Show


class InterpretRepository m where
  interpret :: Repository a -> m a


type Repository = Free RepositoryF  


data RepositoryF a
  = GetIp (IpAddr -> a)
  | RecentPolls Int ([PollHeader] -> a)
  | UserPolls IpAddr ([PollHeader] -> a)
  | LoadPoll PollId (Maybe PollData -> a)
  | CreatePoll IpAddr Question [Choice] (PollId -> a)
  | RegisterVote IpAddr PollId ChoiceId a
  deriving Functor
  
