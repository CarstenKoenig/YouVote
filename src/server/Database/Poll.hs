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
import           Data.Set (Set)
import qualified Data.Set as Set

import qualified Database.Persist as Sql
import qualified Database.Persist.Sql as Sql
import           Database.Persist.Sql ((==.), SqlPersistT, ConnectionPool)

import qualified Database.Model as Db

import           Servant

import qualified Poll.Algebra as Alg
import           Poll.Algebra (PollId, ChoiceId, IpAddr, PollHeader(..), PollData(..), PollChoice(..))


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
interpretSql (Alg.RecentPolls cnt contWith) =
  Reader.lift (listPolls cnt) >>= contWith
interpretSql (Alg.UserPolls ip contWith) =
  Reader.lift (listUserPolls ip) >>= contWith
interpretSql (Alg.LoadPoll pId contWith) =
  Reader.lift (loadPoll pId) >>= contWith
interpretSql (Alg.CreatePoll ip quest cs contWith) = do
  pollKey <- Reader.lift $ Sql.insert $ Db.Poll quest ip
  mapM_ (insertChoice pollKey) cs
  contWith (Sql.fromSqlKey pollKey)
  where
    insertChoice key ans =
      Reader.lift $ Sql.insert $ Db.Choice ans key
interpretSql (Alg.RegisterVote ip pId cId cont) =
  Reader.lift (Sql.insert (Db.Vote (Sql.toSqlKey pId) (Sql.toSqlKey cId) ip)) >> cont


listPolls :: forall m . MonadIO m => Int-> SqlPersistT m [PollHeader]
listPolls cnt = do
  entities <- Sql.selectList
             [ ]
             [ Sql.Desc Db.PollId, Sql.LimitTo cnt ]
  return $ map createPollHeader entities


listUserPolls :: forall m . MonadIO m => IpAddr -> SqlPersistT m [PollHeader]
listUserPolls ipAdr = do
  entities <- Sql.selectList
             [ Db.PollCreatorIP ==. ipAdr ]
             [ Sql.Desc Db.PollId ]
  return $ map createPollHeader entities
      

createPollHeader :: Sql.Entity Db.Poll -> PollHeader
createPollHeader row =
  let pId = Sql.fromSqlKey (Sql.entityKey row)
      quest = Db.pollQuestion (Sql.entityVal row)
      creator = Db.pollCreatorIP (Sql.entityVal row)
  in PollHeader pId quest creator

  

loadPoll :: MonadIO m => PollId -> SqlPersistT m (Maybe PollData)
loadPoll pId = do
  record <- Sql.get (Sql.toSqlKey pId)
  case record of
    Nothing -> return Nothing
    Just pr -> do
      let quest = Db.pollQuestion pr
      let creator = Db.pollCreatorIP pr
      cs <- loadChoices pId
      return . Just $ PollData pId quest creator cs


loadChoices :: MonadIO m => PollId -> SqlPersistT m (Map ChoiceId PollChoice)
loadChoices pId = do
  entities <- Sql.selectList
             [ Db.ChoicePoll ==. Sql.toSqlKey pId ]
             [ Sql.Asc Db.ChoiceId ]
  Map.fromList <$> mapM createChoice entities
  
  where
    createChoice row = do
      votes <- getVotes (Sql.entityKey row)
      let ans = Db.choiceAnswer (Sql.entityVal row)
      let cId = Sql.fromSqlKey (Sql.entityKey row)
      return (cId, PollChoice pId cId ans votes)


getVotes :: MonadIO m => Db.Key Db.Choice -> SqlPersistT m (Set IpAddr)
getVotes choiceKey = do
  rows <- Sql.selectList
             [ Db.VoteChoice ==. choiceKey ]
             []
  let ips = map (Db.voteVoterIP . Sql.entityVal) rows
  return $ Set.fromList ips
