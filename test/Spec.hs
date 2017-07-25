{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Monad.State as State
import           Control.Monad.State (StateT)

import           Data.Proxy (Proxy(..))

import           Network.Wai (Application)
import           Servant (enter, (:~>)(Nat), serve, Handler)

import Lib (server, Routes)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON


main :: IO ()
main = hspec spec


testApp :: TestState -> Application
testApp state =
  serve (Proxy :: Proxy Routes)
  $ server (testToHandler state)


type TestHandler = StateT TestState Handler


type TestState = ()


spec :: Spec
spec = with (return $ testApp ()) $ do
    describe "GET /" $ do
        it "responds with 200" $
            get "/" `shouldRespondWith` 200



testToHandler :: TestState ->  TestHandler :~> Handler
testToHandler state = Nat (testToHandler' state)
  where
    testToHandler' :: forall a. TestState -> TestHandler a -> Handler a
    testToHandler' state th = State.evalStateT th state

