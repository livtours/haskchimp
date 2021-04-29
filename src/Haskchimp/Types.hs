{- |
Copyright: (c) 2021 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskchimp.Types where

import           Data.Aeson
import           Data.Text
import           GHC.Generics      (Generic)
import           Text.Email.Parser (EmailAddress)

newtype JourneyId = JourneyId Int
newtype StepId = StepId Int


-- Mailchimp types

data MError =
  MError { status :: Int
         , title :: String
         , detail :: String
         }
  deriving (Eq, Show, Generic, FromJSON)

data MResponse a = MFail MError | MSuccess a
  deriving (Eq, Show)

-- | Get information about all lists (audiences) in the account.
data List =
  List { id     :: Text
       , web_id :: Int
       , name   :: Text
       }
  deriving (Eq, Show, Generic, FromJSON)

data Lists =
  Lists { lists       :: [List]
        , total_items :: Int
        }
  deriving (Eq, Show, Generic, FromJSON)

data JourneyTriggerPayload =
  JourneyTriggerPayload { email_address :: EmailAddress }
  deriving (Eq, Show, Generic)

instance ToJSON JourneyTriggerPayload where
  toJSON (JourneyTriggerPayload e) = object [ "email_address" .= show e, "foo" .= ("test test" :: String) ]
