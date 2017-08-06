{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Application
    ( startApp
    , app
    , server
    , Routes
    , API
    ) where

import           Control.Exception.Lifted (SomeException, handle)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Sql (runMigration, runSqlPool)
import           Database.Persist.Sqlite (withSqlitePool)

import qualified Lucid as Html
import           Lucid (Html)
import           Network.HTTP.Types (hLocation)
import           Network.Socket (SockAddr(..), hostAddressToTuple, hostAddress6ToTuple)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.HTML.Lucid(HTML)

import qualified Poll
import           Poll (IpAddr, PollId, ChoiceId, PollDescription, PollVote, PollStat, CreatePoll)
import           Database.Poll as Db
import qualified Database.Model as Db

----------------------------------------------------------------------
-- Type-Level part of the Servant app: 

type Routes =
  "static" :> Raw
  :<|> API
  :<|> Pages


type Pages =
  CaptureAll "segments" Text :> Get '[HTML] (Html ())


type API = RemoteHost :> "api" :> 
  ("polls" :> "recent" :> Get '[JSON] [PollDescription]
  :<|> "polls" :> "my" :> Get '[JSON] [PollDescription]
  :<|> "poll" :> Capture "pollId" PollId :> Get '[JSON] PollVote
  :<|> "poll" :> Capture "pollId" PollId :> "stats" :> Get '[JSON] PollStat
  :<|> "poll" :> Capture "pollId" PollId
              :> "vote" :> Capture "choiceId" ChoiceId :> Post '[JSON] PollStat
  :<|> "poll" :> "create" :> ReqBody '[JSON] CreatePoll :> Put '[JSON] PollVote)


----------------------------------------------------------------------
-- application section


localDb :: Text
localDb = "./polls.db"


-- | run the Servant application on localhost:8080
startApp :: IO ()
startApp = do
  putStrLn "running application on http://localhost:8080"
  -- use a sqlite-pool on database file in localDb (with logging to stdout)
  runStdoutLoggingT $ withSqlitePool localDb 5 $ \ pool -> do
    -- run migrations if any
    runSqlPool (runMigration Db.migrateAll) pool
    -- then run the WAI application logging to stdout with a DbHandler using the pool
    liftIO $ run 8080 $ simpleCors $ logStdoutDev $ app pool


-- | the Servant-Application as a WAI Application
app :: ConnectionPool -> Application
app pool =
  serve (Proxy :: Proxy Routes) (server (toDbHandler pool))


-- | the Servant-Server of the Application
server :: (Poll.InterpretRepository m, MonadBaseControl IO m 
          , Monad m, MonadError ServantErr m) => (IpAddr -> m :~> Handler) -> Server Routes
server embedd =
  serveDirectory "static"
  :<|> apiServer embedd
  :<|> pagesServer


----------------------------------------------------------------------
-- Rest API section

-- apiServer can `throwError` so we need the MonadError instance

apiServer :: ( Poll.InterpretRepository m, MonadBaseControl IO m
             , Monad m, MonadError ServantErr m) => (IpAddr -> m :~> Handler) -> Server API
apiServer embedd remoteAddr =
  enter (embedd $ getAdrPart remoteAddr) $
      listRecentPollsHandler
      :<|> listUserPollsHandler
      :<|> getPollHandler
      :<|> getPollStatHandler
      :<|> votePollHandler
      :<|> createPollHandler


listRecentPollsHandler :: (Poll.InterpretRepository m) => m [PollDescription]
listRecentPollsHandler =
  Poll.interpret $ Poll.recentPollsList 5


listUserPollsHandler :: (Poll.InterpretRepository m) => m [PollDescription]
listUserPollsHandler =
  Poll.interpret Poll.userPollsList


getPollHandler :: (MonadError ServantErr m, Poll.InterpretRepository m)
               => PollId -> m PollVote
getPollHandler pId = do
  found <- Poll.interpret $ Poll.loadPollChoices pId
  case found of
    Right poll -> pure poll
    Left err -> throwError (badRequest $ BSL.pack err)
  where
    badRequest err =
      err400 { errBody = err }


getPollStatHandler :: (MonadError ServantErr m, Poll.InterpretRepository m)
               => PollId -> m PollStat
getPollStatHandler pId = do
  found <- Poll.interpret $ Poll.loadPollStats pId
  case found of
    Right poll -> pure poll
    Left err -> throwError (badRequest $ BSL.pack err)
  where
    badRequest err =
      err400 { errBody = err }
      


votePollHandler :: (Poll.InterpretRepository m, MonadError ServantErr m, MonadBaseControl IO m)
                => PollId -> ChoiceId -> m PollStat
votePollHandler pId cId =
  handle (\ (_ :: SomeException) -> throwError $ badRequest "sqlError") $ do
    res <- Poll.interpret $ Poll.voteFor pId cId
    case res of
      Left err -> throwError (badRequest $ BSL.pack err)
      Right () -> redirect $ "/api/poll/" `BS.append` BSC.pack (show pId) `BS.append` "/stats"
  where 
    badRequest err =
      err400 { errBody = err }

      
createPollHandler :: ( MonadError ServantErr m, MonadBaseControl IO m, Poll.InterpretRepository m)
                  => CreatePoll -> m PollVote
createPollHandler newpoll = do
  pollId <- Poll.interpret $ Poll.newPoll newpoll
  redirect $ "/api/poll/" `BS.append` BSC.pack (show pollId)
      

redirect :: (MonadError ServantErr m, MonadBaseControl IO m) => ByteString -> m a
redirect = throwError . redirectRes
  where 
    redirectRes url =
      err303 {  errReasonPhrase = "created"
             , errBody = "created"
             , errHeaders = [(hLocation, url)]
             }      

----------------------------------------------------------------------
-- Html Pages

-- | Servant-Server just returns the Home-page
-- it actually just wraps a static html content so
-- it only needs a general monad
pagesServer :: Monad m => ServerT Pages m
pagesServer = const (return homePage)


homePage :: Html ()
homePage = do
  Html.doctype_ 
  Html.html_ [ Html.lang_ "en" ] $ do
    Html.head_ $ do
      Html.link_ [ Html.rel_ "shortcut icon", Html.href_ "/static/favicon.ico" ]
      Html.meta_ [ Html.charset_ "utf-8" ]
      Html.meta_ [ Html.name_ "description"
                 , Html.content_ "quick voting app demo" ]
      Html.meta_ [ Html.name_ "author"
                 , Html.content_ "Carsten König" ]
      Html.meta_ [ Html.httpEquiv_ "X-UA-Compatible"
                 , Html.content_ "IE=edge" ]
      Html.meta_ [ Html.name_ "viewport"
                 , Html.content_ "width=device-width, initial-scale=1" ]
        
      Html.title_ "YouVote"
      
      -- Bootstrap
      Html.link_ [ Html.href_ "/static/css/bootstrap.min.css"
                 , Html.rel_ "stylesheet" ]
      -- Custom css
      Html.link_ [ Html.href_ "/static/css/site.css"
                 , Html.rel_ "stylesheet" ]

    Html.body_ $
      Html.div_ [ Html.class_ "container" ] elmApp


elmApp :: Html ()
elmApp = do
  Html.div_ [ Html.id_ "main" ] (pure ())
  Html.script_ [ Html.src_ "/static/js/client.js" ] ("" :: Text)
  Html.script_ script
  where
    script :: Text
    script =
      Text.concat
      [ "var node = document.getElementById('main');"
      , "var app = Elm.Main.embed(node, { baseUrl:  'http://localhost:8080' });"
      ]
  

----------------------------------------------------------------------
-- helpers


-- | renders the Address part of a SockAddr into a string
-- mainly used to get IP Adresses in the 192.168.1.10 form
-- this function is used to give an voter-Identifier from
-- the remote host to check if the user already voted on a
-- poll
getAdrPart :: SockAddr -> String
getAdrPart (SockAddrInet _ adr) =
  formatAddr (hostAddressToTuple adr)
  where
    formatAddr (a,b,c,d) =
      intercalate "." . map show $ [a,b,c,d]
getAdrPart (SockAddrInet6 _ _ adr _) =
  formatAddr6 (hostAddress6ToTuple adr)
  where
    formatAddr6 (a,b,c,d,e,f,g,h) =
      intercalate "." . map show $ [a,b,c,d,e,f,g,h]
getAdrPart (SockAddrUnix n) = n
getAdrPart (SockAddrCan nr) = show nr
