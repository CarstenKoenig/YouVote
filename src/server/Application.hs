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
import qualified Data.ByteString.Char8 as BSC
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
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.HTML.Lucid(HTML)

import qualified Poll.Algebra as Alg
import           Poll.Models
import           Database.Poll
import qualified Database.Model as Db

----------------------------------------------------------------------
-- Type-Level part of the Servant app: 

type Routes =
  "static" :> Raw
  :<|> API
  :<|> Pages


type Pages =
  CaptureAll "segments" Text :> Get '[HTML] (Html ())


type API = "api" :>
  ("poll" :> "list" :> Get '[JSON] [Poll]
  :<|> "poll" :> Capture "pollId" PollId :> Get '[JSON] Poll
  :<|> "poll" :> Capture "pollId" PollId
              :> "vote" :> Capture "choiceId" ChoiceId
              :> RemoteHost :> Post '[JSON] ()
  :<|> "poll" :> "create" :> ReqBody '[JSON] CreatePoll :> Put '[JSON] Poll)


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
    liftIO $ run 8080 $ logStdoutDev $ app pool


-- | the Servant-Application as a WAI Application
app :: ConnectionPool -> Application
app pool = serve (Proxy :: Proxy Routes) (server (toDbHandler pool))


-- | the Servant-Server of the Application
server :: (Alg.InterpretRepository m, MonadBaseControl IO m
          , Monad m, MonadError ServantErr m) => (m :~> Handler) -> Server Routes
server embedd =
  serveDirectory "static"
  :<|> enter embedd apiServer
  :<|> enter embedd pagesServer


----------------------------------------------------------------------
-- Rest API section

-- apiServer can `throwError` so we need the MonadError instance

apiServer :: ( Alg.InterpretRepository m, MonadBaseControl IO m
             , Monad m, MonadError ServantErr m) => ServerT API m
apiServer =
  listPollsHandler
  :<|> getPollHandler
  :<|> votePollHandler
  :<|> createPollHandler


listPollsHandler :: (Alg.InterpretRepository m) => m [Poll]
listPollsHandler =
  Alg.interpret (Alg.recentPolls 5)


getPollHandler :: (MonadError ServantErr m, Alg.InterpretRepository m)
               => PollId -> m Poll
getPollHandler pId = do
  found <- Alg.interpret (Alg.loadPoll pId)
  case found of
    Just poll -> pure poll
    Nothing -> throwError notFound
  where
    notFound =
      err404 { errBody = "poll not found" }


votePollHandler :: (Alg.InterpretRepository m
                   , MonadError ServantErr m, MonadBaseControl IO m)
                => PollId -> ChoiceId -> SockAddr -> m ()
votePollHandler pId cId remoteAdr =
  handle (\ (_ :: SomeException) -> throwError badRequest) $ do
    Alg.interpret $ Alg.voteFor (getAdrPart remoteAdr) pId cId
    redirect $ "/api/poll/" `BS.append` BSC.pack (show pId)
  where
    redirect url = throwError (redirectRes url)
    badRequest =
      err400 { errBody = "vote already cast" }
    redirectRes url =
      err303 { errBody = "redirected"
             , errHeaders = [(hLocation, url)]
             }


createPollHandler :: Alg.InterpretRepository m => CreatePoll -> m Poll
createPollHandler newpoll =
  Alg.interpret (Alg.newPoll newpoll)
      

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
      , "var app = Elm.Main.embed(node);"
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
