{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
module Poll.Algebra
  ( Repository
  , RepositoryF (..)
  , InterpretRepository (..)
  , IpAddr
  , recentPolls, loadPoll, newPoll, voteFor
  ) where


import qualified Control.Monad.Free as Free
import           Control.Monad.Free (Free)

import           Poll.Models


type IpAddr = String


class InterpretRepository m where
  interpret :: Repository a -> m a


recentPolls :: Int -> Repository [Poll]
recentPolls count = do
  ip <- getIp
  Free.liftF $ RecentPolls ip count id


loadPoll :: PollId -> Repository (Maybe Poll)
loadPoll pollId = do
  ip <- getIp
  Free.liftF $ LoadPoll ip pollId id


newPoll :: CreatePoll -> Repository PollId
newPoll poll = do
  ip <- getIp
  Free.liftF $ NewPoll ip poll id


voteFor :: PollId -> ChoiceId -> Repository ()
voteFor pollId choiceId = do
  ip <- getIp
  Free.liftF $ VoteFor ip pollId choiceId ()


getIp :: Repository IpAddr
getIp = Free.liftF $ GetIp id

type Repository = Free RepositoryF  


data RepositoryF a
  = LoadPoll IpAddr PollId (Maybe Poll -> a)
  | NewPoll IpAddr CreatePoll (PollId -> a)
  | VoteFor IpAddr PollId ChoiceId a
  | RecentPolls IpAddr Int ([Poll] -> a)
  | GetIp (IpAddr -> a)
  deriving Functor
  
