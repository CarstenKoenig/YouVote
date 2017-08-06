{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Monad.Free as Free
import qualified Control.Monad.State as State
import           Control.Monad.State (StateT)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Proxy (Proxy(..))

import           Network.Wai (Application)
import           Servant (enter, (:~>)(Nat), serve, Handler)

import Application (server, Routes)

import Poll.Algebra
import Poll.Models

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON


main :: IO ()
main = hspec spec


testApp :: PollStoreState -> Application
testApp store =
  serve (Proxy :: Proxy Routes)
  $ server (toHandler store)
  

spec :: Spec
spec = with (return $ testApp $ emptyStore "127.0.0.1") $
    describe "GET /" $
        it "responds with 200" $
            get "/" `shouldRespondWith` 200



toHandler :: PollStoreState ->  IpAddr -> StateT PollStoreState Handler :~> Handler
toHandler store ip = Nat (toHandler' store ip)
  where
    toHandler' :: forall a. PollStoreState -> IpAddr -> PollStore Handler a -> Handler a
    toHandler' state ip th = State.evalStateT th state



----------------------------------------------------------------------

type PollStore m a = StateT PollStoreState m a 


data PollStoreState =
  PollStoreState
  { polls :: Map PollId PollData
  , ip :: IpAddr
  }


emptyStore :: IpAddr -> PollStoreState
emptyStore = PollStoreState Map.empty


instance Monad m => InterpretRepository (StateT PollStoreState m) where
  interpret = Free.iterM iterRep
    where
    iterRep (GetIp cont) = do
      ipAdr <- State.gets ip
      cont ipAdr
    iterRep (RecentPolls cnt contWith) = do
      polls <- take cnt . reverse <$> State.gets (map toHeader . Map.elems . polls)
      contWith polls
    iterRep (LoadPoll pollId contWith) = do
      found <- State.gets (Map.lookup pollId . polls)
      contWith found
    iterRep (Poll.Algebra.CreatePoll ip quest cs contWith) = do
      nextPId <- nextPollId
      nextCId <- nextChoiceId
      let
        choices = Map.fromList $ zipWith
          (\cT cId -> (cId, PollChoice nextPId cId cT Set.empty))
          cs
          [nextCId..]
        added = PollData nextPId quest ip choices
      State.modify (\s -> s { polls = Map.insert nextPId added (polls s) })
      contWith nextPId
    iterRep (RegisterVote ip pollId choiceId cont) = do
      choice <- getChoice pollId choiceId
      cont


toHeader :: PollData -> PollHeader
toHeader (PollData pId quest creator _) = PollHeader pId quest creator


getChoice :: Monad m => PollId -> ChoiceId -> PollStore m (Maybe PollChoice)
getChoice pollId choiceId  = do
  poll <- State.gets (Map.lookup pollId . polls)
  return $ poll >>= (Map.lookup choiceId . pdChoices)



nextPollId :: Monad m => PollStore m PollId
nextPollId = do
  count <- State.gets (Map.size . polls)
  return . fromIntegral $ count + 1


nextChoiceId :: Monad m => PollStore m ChoiceId
nextChoiceId = do
  pMap <- State.gets polls
  return . fromIntegral
    $ Map.foldl' (\sum p -> sum + Map.size (pdChoices p)) 0 pMap
