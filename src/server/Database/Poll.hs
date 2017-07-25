{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Database.Poll
  ( ConnectionPool
  , DbHandler
  , toDbHandler
  ) where

import           Control.Monad (mapM, mapM_)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Free as Free
import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Reader (ReaderT)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)

import qualified Database.Persist as Sql
import qualified Database.Persist.Sql as Sql
import           Database.Persist.Sql ((==.), SqlPersistT, ConnectionPool)

import qualified Database.Model as Db

import           Servant

import           Poll.Algebra
import           Poll.Models


type DbHandler = ReaderT ConnectionPool Handler

toDbHandler :: ConnectionPool -> ReaderT ConnectionPool Handler :~> Handler
toDbHandler pool = Nat (toHandler' pool)
  where
    toHandler' :: forall a. ConnectionPool -> ReaderT ConnectionPool Handler a -> Handler a
    toHandler' pool th = Reader.runReaderT th pool


instance (MonadBaseControl IO m, MonadIO m) =>
         InterpretRepository (ReaderT ConnectionPool m) where
  interpret rep = do
    pool <- Reader.ask
    let query = Free.iterM interpretSql rep
    Sql.runSqlPool query pool


interpretSql :: MonadIO m =>
                RepositoryF (SqlPersistT m a) ->
                SqlPersistT m a
interpretSql (LoadPoll pollId contWith) =
  loadPoll pollId >>= contWith
interpretSql (NewPoll poll contWith) = do
  pollKey <- Sql.insert $ Db.Poll (newQuestion poll)
  mapM_ (insertChoice pollKey) (newChoices poll)
  p <- loadPoll (Sql.fromSqlKey pollKey)
  contWith (fromJust p)
  where
    insertChoice key answer =
      Sql.insert $ Db.Choice answer key
interpretSql (VoteFor ip _ choiceId cont) =
  Sql.insert (Db.Vote (Sql.toSqlKey choiceId) ip) >> cont


loadPoll :: MonadIO m => PollId -> SqlPersistT m (Maybe Poll)
loadPoll pollId = do
  record <- Sql.get (Sql.toSqlKey pollId)
  case record of
    Nothing -> return Nothing
    Just pr -> do
      let question = Db.pollQuestion pr
      cs <- loadChoices pollId
      return . Just $ Poll pollId question cs


loadChoices :: MonadIO m => PollId -> SqlPersistT m (Map ChoiceId PollChoice)
loadChoices pollId = do
  entities <- Sql.selectList
             [ Db.ChoicePoll ==. Sql.toSqlKey pollId ]
             [ Sql.Asc Db.ChoiceId ]
  Map.fromList <$> mapM createChoice entities
  
  where
    createChoice c = do
      nrVotes <- countVotes (Sql.entityKey c)
      let answer = Db.choiceAnswer (Sql.entityVal c)
      let id = Sql.fromSqlKey (Sql.entityKey c)
      return (id, PollChoice id answer (Just nrVotes))


countVotes :: MonadIO m => Db.Key Db.Choice -> SqlPersistT m Int
countVotes choiceKey =
  length <$> Sql.selectList
             [ Db.VoteChoice ==. choiceKey ]
             []
