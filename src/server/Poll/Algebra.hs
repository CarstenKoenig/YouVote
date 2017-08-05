{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
module Poll.Algebra
  ( Repository
  , RepositoryF (..)
  , InterpretRepository (..)
  , recentPolls, loadPoll, newPoll, voteFor
  ) where


import qualified Control.Monad.Free as Free
import           Control.Monad.Free (Free)

import           Poll.Models


class InterpretRepository m where
  interpret :: String -> Repository a -> m a


recentPolls :: Int -> Repository [Poll]
recentPolls count =
  Free.liftF $ RecentPolls count id

loadPoll :: PollId -> Repository (Maybe Poll)
loadPoll pollId =
  Free.liftF $ LoadPoll pollId id


newPoll :: CreatePoll -> Repository PollId
newPoll poll =
  Free.liftF $ NewPoll poll id


voteFor :: PollId -> ChoiceId -> Repository ()
voteFor pollId choiceId =
  Free.liftF $ VoteFor pollId choiceId ()

type Repository = Free RepositoryF  


data RepositoryF a
  = LoadPoll PollId (Maybe Poll -> a)
  | NewPoll CreatePoll (PollId -> a)
  | VoteFor PollId ChoiceId a
  | RecentPolls Int ([Poll] -> a)
  deriving Functor
  
