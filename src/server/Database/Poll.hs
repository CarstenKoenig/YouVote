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

import qualified Poll.Algebra as Alg
import           Poll.Models


type DbHandler = ReaderT ConnectionPool Handler

toDbHandler :: ConnectionPool -> ReaderT ConnectionPool Handler :~> Handler
toDbHandler pool = Nat (toHandler' pool)
  where
    toHandler' :: forall a. ConnectionPool -> ReaderT ConnectionPool Handler a -> Handler a
    toHandler' p th = Reader.runReaderT th p


instance (MonadBaseControl IO m, MonadIO m) =>
         Alg.InterpretRepository (ReaderT ConnectionPool m) where
  interpret rep = do
    pool <- Reader.ask
    let query = Free.iterM interpretSql rep
    Sql.runSqlPool query pool


interpretSql :: MonadIO m =>
                Alg.RepositoryF (SqlPersistT m a) ->
                SqlPersistT m a
interpretSql (Alg.LoadPoll pId contWith) =
  loadPoll pId >>= contWith
interpretSql (Alg.NewPoll poll contWith) = do
  pollKey <- Sql.insert $ Db.Poll (newQuestion poll)
  mapM_ (insertChoice pollKey) (newChoices poll)
  p <- loadPoll (Sql.fromSqlKey pollKey)
  contWith (fromJust p)
  where
    insertChoice key ans =
      Sql.insert $ Db.Choice ans key
interpretSql (Alg.VoteFor ip pId cId cont) =
  Sql.insert (Db.Vote (Sql.toSqlKey pId) (Sql.toSqlKey cId) ip) >> cont


loadPoll :: MonadIO m => PollId -> SqlPersistT m (Maybe Poll)
loadPoll pId = do
  record <- Sql.get (Sql.toSqlKey pId)
  case record of
    Nothing -> return Nothing
    Just pr -> do
      let q = Db.pollQuestion pr
      cs <- loadChoices pId
      return . Just $ Poll pId q cs


loadChoices :: MonadIO m => PollId -> SqlPersistT m (Map ChoiceId PollChoice)
loadChoices pId = do
  entities <- Sql.selectList
             [ Db.ChoicePoll ==. Sql.toSqlKey pId ]
             [ Sql.Asc Db.ChoiceId ]
  Map.fromList <$> mapM createChoice entities
  
  where
    createChoice c = do
      nrVotes <- countVotes (Sql.entityKey c)
      let ans = Db.choiceAnswer (Sql.entityVal c)
      let cId = Sql.fromSqlKey (Sql.entityKey c)
      return (cId, PollChoice cId ans (Just nrVotes))


countVotes :: MonadIO m => Db.Key Db.Choice -> SqlPersistT m Int
countVotes choiceKey =
  length <$> Sql.selectList
             [ Db.VoteChoice ==. choiceKey ]
             []
