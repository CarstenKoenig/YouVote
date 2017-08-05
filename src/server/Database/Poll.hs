{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Database.Persist as Sql
import qualified Database.Persist.Sql as Sql
import           Database.Persist.Sql ((==.), SqlPersistT, ConnectionPool)

import qualified Database.Model as Db

import           Servant

import qualified Poll.Algebra as Alg
import           Poll.Algebra (IpAddr)
import           Poll.Models


type DbHandler = ReaderT ConnectionPool Handler

toDbHandler :: ConnectionPool -> IpAddr -> ReaderT (ConnectionPool, IpAddr) Handler :~> Handler
toDbHandler pool ip = Nat $ toHandler' (pool, ip)
  where
    toHandler' :: forall a. (ConnectionPool, IpAddr) -> ReaderT (ConnectionPool, IpAddr) Handler a -> Handler a
    toHandler' (p,i) th = Reader.runReaderT th (p,i)


instance (MonadBaseControl IO m, MonadIO m) =>
         Alg.InterpretRepository (ReaderT (ConnectionPool, IpAddr) m) where
  interpret rep = do
    (pool, ip) <- Reader.ask
    let query = Reader.runReaderT (Free.iterM interpretSql rep) ip
    Sql.runSqlPool query pool


interpretSql :: MonadIO m =>
 Alg.RepositoryF (ReaderT IpAddr (SqlPersistT m) a) ->
                ReaderT IpAddr (SqlPersistT m) a
interpretSql (Alg.GetIp cont) = do
  ip <- Reader.ask
  cont ip
interpretSql (Alg.RecentPolls ip cnt contWith) =
  Reader.lift (listPolls ip cnt) >>= contWith
interpretSql (Alg.LoadPoll ip pId contWith) =
  Reader.lift (loadPoll pId ip) >>= contWith
interpretSql (Alg.NewPoll ip poll contWith) = do
  pollKey <- Reader.lift $ Sql.insert $ Db.Poll (newQuestion poll)
  mapM_ (insertChoice pollKey) (newChoices poll)
  contWith (Sql.fromSqlKey pollKey)
  where
    insertChoice key ans =
      Reader.lift $ Sql.insert $ Db.Choice ans key
interpretSql (Alg.VoteFor ip pId cId cont) =
  Reader.lift (Sql.insert (Db.Vote (Sql.toSqlKey pId) (Sql.toSqlKey cId) ip)) >> cont


listPolls :: forall m . MonadIO m => String -> Int-> SqlPersistT m [Poll]
listPolls ip cnt = do
  entities <- Sql.selectList
             [ ]
             [ Sql.Desc Db.PollId, Sql.LimitTo cnt ]
  mapM createPoll entities
  where
    createPoll p = do
      let pId = Sql.fromSqlKey (Sql.entityKey p)
      let q = Db.pollQuestion (Sql.entityVal p)
      cs <- loadChoices pId ip
      return $ Poll pId q cs
  

loadPoll :: MonadIO m => PollId -> String -> SqlPersistT m (Maybe Poll)
loadPoll pId ip = do
  record <- Sql.get (Sql.toSqlKey pId)
  case record of
    Nothing -> return Nothing
    Just pr -> do
      let q = Db.pollQuestion pr
      cs <- loadChoices pId ip
      return . Just $ Poll pId q cs


loadChoices :: MonadIO m => PollId -> String -> SqlPersistT m (Map ChoiceId PollChoice)
loadChoices pId ip = do
  voted <- hasVoted pId ip
  entities <- Sql.selectList
             [ Db.ChoicePoll ==. Sql.toSqlKey pId ]
             [ Sql.Asc Db.ChoiceId ]
  Map.fromList <$> mapM (createChoice voted) entities
  
  where
    createChoice inclVotes c = do
      nrVotes <- if inclVotes then Just <$> countVotes (Sql.entityKey c) else pure Nothing
      let ans = Db.choiceAnswer (Sql.entityVal c)
      let cId = Sql.fromSqlKey (Sql.entityKey c)
      return (cId, PollChoice cId ans nrVotes)


hasVoted :: MonadIO m => PollId -> String -> SqlPersistT m Bool
hasVoted pId ip =
  not . null <$> Sql.selectList [ Db.VotePoll ==. Sql.toSqlKey pId, Db.VoteVoterIP ==. ip ] []
  

countVotes :: MonadIO m => Db.Key Db.Choice -> SqlPersistT m Int
countVotes choiceKey =
  length <$> Sql.selectList
             [ Db.VoteChoice ==. choiceKey ]
             []
