{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Poll
  ( recentPollsList
  , userPollsList
  , loadPollStats
  , loadPollChoices
  , voteFor
  , newPoll
  , interpret
  , Repository
  , InterpretRepository
  , PollId, ChoiceId, IpAddr
  , PollDescription (..)
  , PollStat (..), Stat (..)
  , PollVote (..)
  , CreatePoll (..)
  ) where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


import           Poll.Models
import           Poll.Algebra



recentPollsList :: Int -> Repository [PollDescription]
recentPollsList cnt =
  map headerToDescription <$> recentPolls cnt


userPollsList :: Repository [PollDescription]
userPollsList = do
  ipAdr <- getIp
  map headerToDescription <$> userPolls ipAdr


loadPollStats :: PollId -> Repository (Either String PollStat)
loadPollStats pId = do
  ipAdr <- getIp
  pData <- loadPoll pId
  case pData of
    Nothing -> pure $ Left "poll not found"
    Just pData ->
      if isAuthorized ipAdr pData
      then pure $ Right $ dataToStats pData
      else pure $ Left "not authorized to see votes"
  where
    isAuthorized ipAdr pData =
      isCreator ipAdr pData || hasVoted ipAdr pData



loadPollChoices :: PollId -> Repository (Either String PollVote)
loadPollChoices pId = do
  ipAdr <- getIp
  pData <- loadPoll pId
  case pData of
    Nothing -> pure $ Left "poll not found"
    Just pData ->
      if isAuthorized ipAdr pData
      then pure $ Right $ dataToVote pData
      else pure $ Left "not authorized to vote"
  where
    isAuthorized ipAdr pData =
      not $ hasVoted ipAdr pData


voteFor :: PollId -> ChoiceId -> Repository (Either String ())
voteFor pId cId = do
  ipAdr <- getIp
  pData <- loadPoll pId
  case pData of 
    Nothing -> pure $ Left "poll not found"
    Just pData ->
      if isAuthorized ipAdr pData
      then Right <$> registerVote ipAdr pId cId
      else pure $ Left "not authorized to vote"
  where
    isAuthorized ipAdr pData =
      not $ hasVoted ipAdr pData
      


newPoll :: CreatePoll -> Repository PollId
newPoll cP = do
  ipAdr <- getIp
  createPoll ipAdr (newQuestion cP) (newChoices cP)
      


----------------------------------------------------------------------
-- helpers

headerToDescription :: PollHeader -> PollDescription
headerToDescription (PollHeader pId quest _) = PollDescription pId quest


dataToStats :: PollData -> PollStat
dataToStats pData =
  PollStat (Poll.Algebra.pdId pData) (Poll.Algebra.pdQuestion pData) votes
  where
    votes = map choiceToStat (Map.elems $ pdChoices pData)


choiceToStat :: PollChoice -> Stat
choiceToStat pChoice =
  Stat (pcChoice pChoice) nrVotes
  where
    nrVotes = Set.size (pcVotes pChoice)


dataToVote :: PollData -> PollVote
dataToVote pData =
  PollVote (Poll.Algebra.pdId pData) (Poll.Algebra.pdQuestion pData) choices
  where
    choices = pcChoice <$> pdChoices pData


isCreator :: IpAddr -> PollData -> Bool
isCreator ipAdr pData =
  pdCreator pData == ipAdr


hasVoted :: IpAddr -> PollData -> Bool
hasVoted ipAdr pData =
  any (Set.member ipAdr . pcVotes) (Map.elems $ pdChoices pData)
