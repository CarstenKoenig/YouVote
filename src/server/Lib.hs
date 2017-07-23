{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( startApp
    , app
    , Addition (..)
    , API
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics
import qualified Lucid as Html
import           Lucid (Html)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid(HTML)

data Addition = Addition
  { operandA      :: Int
  , operandB      :: Int
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Addition)


----------------------------------------------------------------------
-- Type-Level part of the Servant app: 

type Routes =
  "static" :> Raw
  :<|> Pages
  :<|> API


type Pages =
  Get '[HTML] (Html ())


type API = "add" :> ReqBody '[JSON] Addition :> Post '[JSON] Int


----------------------------------------------------------------------
-- application section


-- | run the Servant application on localhost:8080
startApp :: IO ()
startApp = do
  putStrLn "running application on http://localhost:8080"
  run 8080 app


-- | the Servant-Application as a WAI Application
app :: Application
app = serve (Proxy :: Proxy Routes) server


-- | the Servant-Server of the Application
server :: Server Routes
server =
  serveDirectory "static"
  :<|> pagesServer
  :<|> apiServer


----------------------------------------------------------------------
-- Rest API section

apiServer :: Server API
apiServer = addHandler
  where
    addHandler add =
      pure $ operandA add + operandB add


----------------------------------------------------------------------
-- Html Pages

-- | Servant-Server just returns the Home-page
pagesServer :: Server Pages
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
  
