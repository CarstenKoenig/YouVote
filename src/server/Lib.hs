{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( startApp
    , app
    , server
    , Addition (..)
    , Routes
    , API
    ) where

import           Control.Monad.Error.Class (MonadError)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Lucid as Html
import           Lucid (Html)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.HTML.Lucid(HTML)

import           Poll.Models

----------------------------------------------------------------------
-- Type-Level part of the Servant app: 

type Routes =
  "static" :> Raw
  :<|> Pages
  :<|> API


type Pages =
  Get '[HTML] (Html ())


type API = "api" :>
  ("add" :> ReqBody '[JSON] Addition :> Post '[JSON] Int
  :<|> "poll" :> "list" :> Get '[JSON] [Poll]
  :<|> "poll" :> Capture "pollId" PollId :> Get '[JSON] Poll
  :<|> "poll" :> Capture "pollId" PollId :> "vote" :> Capture "choiceId" ChoiceId :> Post '[JSON] Poll
  :<|> "poll" :> "create" :> ReqBody '[JSON] CreatePoll :> Put '[JSON] Poll)


----------------------------------------------------------------------
-- application section


-- | run the Servant application on localhost:8080
startApp :: IO ()
startApp = do
  putStrLn "running application on http://localhost:8080"
  run 8080 $ logStdoutDev app


-- | the Servant-Application as a WAI Application
app :: Application
app = serve (Proxy :: Proxy Routes) (server (Nat id))


-- | the Servant-Server of the Application
server :: (Monad m, MonadError ServantErr m) => (m :~> Handler) -> Server Routes
server embedd =
  serveDirectory "static"
  :<|> enter embedd pagesServer
  :<|> enter embedd apiServer


----------------------------------------------------------------------
-- Rest API section

-- apiServer can `throwError` so we need the MonadError instance

apiServer :: (Monad m, MonadError ServantErr m) => ServerT API m
apiServer =
  addHandler
  :<|> listPollsHandler
  :<|> getPollHandler
  :<|> votePollHandler
  :<|> createPollHandler
  where
    addHandler add =
      pure $ operandA add + operandB add
    listPollsHandler =
      pure examplePolls
    getPollHandler pollId =
      case lookup (fromIntegral pollId) (zip [1..] examplePolls) of
        Nothing -> throwError notFound
        Just poll -> pure poll
    votePollHandler pollId choiceId =
      undefined
    createPollHandler newPoll =
      undefined
    notFound =
      err404 { errBody = "poll not found" }


examplePolls :: [Poll]
examplePolls =
  [ Poll 1 "What's your favorite programming language?" 
    (toChoiceMap
      [ PollChoice 1 "Haskell" (Just 21)
      , PollChoice 2 "Elm" (Just 13) 
      , PollChoice 3 "JavaScript" (Just 5)
      ])
  , Poll 2 "What's your favorite metal band?"
    (toChoiceMap
     [ PollChoice 4 "Metal what?" Nothing
     , PollChoice 5 "Justin Bieber" Nothing
     ])
  ]
  where toChoiceMap chs =
          Map.fromList (map (\c -> (choiceId c, c)) chs)

----------------------------------------------------------------------
-- Html Pages

-- | Servant-Server just returns the Home-page
-- it actually just wraps a static html content so
-- it only needs a general monad
pagesServer :: Monad m => ServerT Pages m
pagesServer = return homePage


homePage :: Html ()
homePage =
  Html.html_ $
  Html.body_ $ do
     Html.h1_ [] (Html.toHtml ("Hello Servant" :: Text))
     elmApp

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
  
