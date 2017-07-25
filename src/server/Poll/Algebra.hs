{-# LANGUAGE DeriveFunctor #-}
module Poll.Algebra
  ( Repository
  , RepositoryF (..)
  , InterpretRepository (..)
  , interpret
  ) where


import qualified Control.Monad.Free as Free
import           Control.Monad.Free (Free)

import           Poll.Models


class InterpretRepository m where
  iterRep :: RepositoryF (m a) -> m a


interpret :: (InterpretRepository m, Monad m) => Repository a -> m a
interpret = Free.iterM iterRep 


loadPoll :: PollId -> Repository (Maybe Poll)
loadPoll pollId =
  Free.liftF $ LoadPoll pollId id


newPoll :: CreatePoll -> Repository Poll
newPoll poll =
  Free.liftF $ NewPoll poll id


voteFor :: String -> PollId -> ChoiceId -> Repository ()
voteFor ip pollId choiceId =
  Free.liftF $ VoteFor ip pollId choiceId ()

type Repository = Free RepositoryF  


data RepositoryF a
  = LoadPoll PollId (Maybe Poll -> a)
  | NewPoll CreatePoll (Poll -> a)
  | VoteFor String PollId ChoiceId a
  deriving Functor
  