{- |
Copyright: (c) 2021 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Haskchimp.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types      (parseFail)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict   as H
import           Data.Text             hiding (drop, map)
import           Debug.Trace
import           GHC.Generics          (Generic)
import           Text.Email.Parser     (EmailAddress, toByteString)
import           Text.Email.Validate   (emailAddress)

-- | A couple of orphan instances to simplify json parsing/encoding
instance ToJSON EmailAddress where
  toJSON email = String $ pack $ B8.unpack $ toByteString email
instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" $ \t -> do
    traceShowM t
    case emailAddress $ B8.pack $ unpack t of
      Nothing -> parseFail $ "Invalid email address: " <> unpack t
      Just e  -> pure e

-- | A Mailchimp Journey ID
newtype JourneyId = JourneyId Int
-- | A Journey's Step ID
newtype StepId = StepId Int
-- | A List (Audience) ID
newtype ListId = ListId Text
  deriving stock (Show, Generic)
  deriving newtype Eq
  deriving anyclass (FromJSON, ToJSON)

type MergeVars = H.HashMap Text Text

-- | A name for an Event to be sent to Mailchimp
newtype EventName = EventName Text
  deriving stock (Show, Generic)
  deriving newtype Eq
  deriving anyclass (FromJSON, ToJSON)

-- Mailchimp types

-- | Mailchimp's json error model
data MError =
  MError { _status   :: Int
         , _title    :: Text
         , _type     :: Maybe Text
         , _detail   :: Text
         , _instance :: Text
         }
  deriving stock (Eq, Show, Generic)
$(deriveJSON (defaultOptions { fieldLabelModifier = drop 1 }) ''MError)


-- | Mailchimp returning a failure or a success, both in json format.
data MResponse a = MFail MError | MSuccess a
  deriving stock (Show, Eq)

instance Functor MResponse where
  fmap f (MSuccess a) = MSuccess $ f a
  fmap _ (MFail e)    = MFail e

instance Applicative MResponse where
  pure = MSuccess
  (MSuccess ab) <*> (MSuccess a) = MSuccess (ab a)
  MFail e <*> _ = MFail e
  MSuccess ab <*> b = fmap ab b

-- | We might fail parsing the json.
-- | TODO maybe we can conflate all the errors
type MResult a = Either String (MResponse a)

-- LISTS

-- | A list (audience) info
data List =
  List { id     :: ListId
       , web_id :: Int
       , name   :: Text
       }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | All the lists (audiences) of the account
data Lists =
  Lists { lists       :: [List]
        , total_items :: Int
        }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | The email type the customer has chosen
data EmailType = EmailTypeHTML | EmailTypeText
  deriving stock (Eq, Show, Generic)

instance ToJSON EmailType where
  toJSON EmailTypeHTML = toJSON ("html" :: String)
  toJSON EmailTypeText = toJSON ("text" :: String)

instance FromJSON EmailType where
  parseJSON = withText "EmailType"
    $ \case
        "html" -> pure EmailTypeHTML
        "text" -> pure EmailTypeText
        t -> parseFail $ "Invalid email type: " <> unpack t

-- | The list status of a specific address
data ListMemberStatus
  = ListMemberStatusSubscribed
  | ListMemberStatusUnsubscribed
  | ListMemberStatusCleaned
  | ListMemberStatusPending
  deriving stock (Eq, Show, Generic)

instance ToJSON ListMemberStatus where
  toJSON ListMemberStatusSubscribed   = toJSON ("subscribed" :: String)
  toJSON ListMemberStatusUnsubscribed = toJSON ("unsubscribed" :: String)
  toJSON ListMemberStatusCleaned      = toJSON ("cleaned" :: String)
  toJSON ListMemberStatusPending      = toJSON ("pending" :: String)

instance FromJSON ListMemberStatus where
  parseJSON = withText "ListMemberStatus"
    $ \case
      "subscribed" -> pure ListMemberStatusSubscribed
      "unsubscribed" -> pure ListMemberStatusUnsubscribed
      "cleaned" -> pure ListMemberStatusCleaned
      "pending" -> pure ListMemberStatusPending
      t -> parseFail $ "Invalid status: " <> unpack t

-- | payload used by the addUpdateMember function
data MemberUpdatePayload =
  MemberUploadPayload { email_address :: EmailAddress
                      , status_if_new :: ListMemberStatus
                      , email_type    :: EmailType
                      , merge_fields  :: MergeVars
                      , vip           :: Bool
                      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass ToJSON

-- | payload used by the addMember function
data MemberPayload =
  MemberPayload { email_address :: EmailAddress
                , status        :: ListMemberStatus
                , email_type    :: EmailType
                , merge_fields  :: MergeVars
                , vip           :: Bool
                }
  deriving stock (Eq, Show, Generic)
  deriving anyclass ToJSON


-- | The data sent in batch update requests.
data ListBatchUpdatePayload =
  ListBatchUpdatePayload { members         :: [MemberPayload]
                         , update_existing :: Bool
                         -- , skip_merge_validation :: Bool -- TODO this are query params..
                         -- , skip_duplicate_check :: Bool
                         }
  deriving stock (Eq, Show, Generic)
  deriving anyclass ToJSON

data ListMemberResult =
  ListMemberResult { id              :: Text
                   , email_address   :: EmailAddress
                   , unique_email_id :: Text
                   , email_type      :: EmailType
                   , status          :: ListMemberStatus
                   , merge_fields    :: MergeVars
                   , vip             :: Bool
                   , list_id         :: ListId
                   -- more fields we will not need
                   }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The possible error types coming back from a batch list update.
data ListMemberErrorType = ErrorContactExists | ErrorGeneric
  deriving stock (Eq, Show)
instance FromJSON ListMemberErrorType where
  parseJSON = withText "ListMemberErrorType"
    $ \case
      "ERROR_CONTACT_EXISTS" -> pure ErrorContactExists
      "ERROR_GENERIC" -> pure ErrorGeneric
      t -> parseFail $ "Invalid ListMemberErrorType: " <> unpack t

-- | The possible error coming back from a list batch update.
data ListMemberError =
  ListMemberError { email_address :: Text -- TODO this email has escaped quotes
                  , error         :: Text
                  , error_code    :: ListMemberErrorType
                  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass FromJSON

-- | The list batch update response
data ListBatchUpdateResponse =
  ListBatchUpdateResponse { new_members     :: [ListMemberResult]
                          , updated_members :: [ListMemberResult]
                          , errors          :: [ListMemberError]
                          , total_created   :: Int
                          , total_updated   :: Int
                          , error_count     :: Int
                          }
  deriving stock (Eq, Show, Generic)
  deriving anyclass FromJSON

-- JOURNEYS

-- | The data sent in a journey trigger
newtype JourneyTriggerPayload =
  JourneyTriggerPayload { email_address :: EmailAddress }
  deriving stock (Show, Generic)
  deriving newtype Eq
  deriving anyclass ToJSON


 -- EVENTS

-- | The data sent in an event trigger.
data EventPayload a =
  EventPayload { name       :: EventName
               , properties :: MergeVars
               -- , is_syncing :: Bool
               -- , occurred_at :: ISO 8601 date
               }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
