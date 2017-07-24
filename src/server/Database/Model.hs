{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs,ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances #-}
-- jup Persist needs quite some things
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Database.Model where

import           Data.Text (Text)
import           Database.Persist.TH (share, persistLowerCase
                                     , mkPersist, mkMigrate, sqlSettings)
                 

----------------------------------------------------------------------
-- interpret using a Persistent-Sqlite database

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Poll json
    question Text
    deriving Show
Choice json
    answer Text
    poll PollId
    deriving Show
Vote json
    choice ChoiceId
    voterIP String
    deriving Show
|]

