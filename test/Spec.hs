{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Monad.Free as Free
import qualified Control.Monad.State as State
import           Control.Monad.State (StateT)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy(..))

import           Network.Wai (Application)
import           Servant (enter, (:~>)(Nat), serve, Handler)

import Lib (server, Routes)

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
spec = with (return $ testApp emptyStore) $ do
    describe "GET /" $ do
        it "responds with 200" $
            get "/" `shouldRespondWith` 200



toHandler :: PollStoreState ->  (StateT PollStoreState Handler) :~> Handler
toHandler store = Nat (toHandler' store)
  where
    toHandler' :: forall a. PollStoreState -> PollStore Handler a -> Handler a
    toHandler' state th = State.evalStateT th state



----------------------------------------------------------------------

type PollStore m a = StateT PollStoreState m a


data PollStoreState =
  PollStoreState
  { polls :: Map PollId Poll
  }


emptyStore :: PollStoreState
emptyStore = PollStoreState Map.empty


instance Monad m => InterpretRepository (StateT PollStoreState m) where
  interpret = Free.iterM iterRep
    where
    iterRep (LoadPoll pollId contWith) = do
      found <- State.gets (Map.lookup pollId . polls)
      contWith found
    iterRep (NewPoll poll contWith) = do
      nextPId <- nextPollId
      nextCId <- nextChoiceId
      let
        choices = Map.fromList $ zipWith
          (\cT cId -> (cId, PollChoice cId cT Nothing))
          (newChoices poll)
          [nextCId..]
        added = Poll nextPId (newQuestion poll) choices
      State.modify (\s -> s { polls = Map.insert nextPId added (polls s) })
      contWith added
    iterRep (VoteFor _ pollId choiceId cont) = do
      choice <- getChoice pollId choiceId
      cont
        

getChoice :: Monad m => PollId -> ChoiceId -> PollStore m (Maybe PollChoice)
getChoice pollId choiceId  = do
  poll <- State.gets (Map.lookup pollId . polls)
  return $ poll >>= (Map.lookup choiceId . choices)



nextPollId :: Monad m => PollStore m PollId
nextPollId = do
  count <- State.gets (Map.size . polls)
  return . fromIntegral $ count + 1


nextChoiceId :: Monad m => PollStore m ChoiceId
nextChoiceId = do
  pMap <- State.gets polls
  return . fromIntegral
    $ Map.foldl' (\sum p -> sum + Map.size (choices p)) 0 pMap
