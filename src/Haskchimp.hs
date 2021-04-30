{- |
Copyright: (c) 2021 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskchimp
  ( module Haskchimp
  , module Haskchimp.Types
  )
where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Digest.Pure.MD5
import           Data.Text                  (unpack)
import           Debug.Trace
import           Haskchimp.Types
import           Network.HTTP.Simple
import           Text.Email.Parser          (EmailAddress)

-- dc is us10 for us
rootUrl :: String -> String
rootUrl dc = "https://" <> dc <> ".api.mailchimp.com/3.0/"

campaignsUrl :: String -> String
campaignsUrl dc = rootUrl dc <> "campaigns/"

listsUrl :: String -> String
listsUrl dc = rootUrl dc <> "lists/"

journeyTriggerUrl :: String -> JourneyId -> StepId -> String
journeyTriggerUrl dc (JourneyId j) (StepId s) =
  rootUrl dc <> "/customer-journeys/journeys/" <> show j <> "/steps/" <> show s <> "/actions/trigger"

addEventUrl :: String -> ListId -> EmailAddress -> String
addEventUrl dc (ListId l) emailAddress =
  let hashedMail = show $ md5 $ L8.pack $ show emailAddress
  in rootUrl dc <> "/lists/" <> unpack l <> "/members/" <> hashedMail <> "/events"

-- | Temporary test key to try stuff out
mailchimpkey :: String
mailchimpkey = "87c4a1f0cf1a91107098c27d073dc790-us10"

handleResponse :: FromJSON a => Response L8.ByteString -> MResult a
handleResponse r = if getResponseStatusCode r >= 400
                   then MFail <$> eitherDecode (getResponseBody r)
                   else MSuccess <$> eitherDecode (getResponseBody r)

-- | Helper method for get requests
get_ :: MonadIO m => FromJSON a => String -> m (MResult a)
get_ url = liftIO $ do
  request' <- parseRequest $ "GET " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> mailchimpkey]
                                 request'
  res <- httpLBS request
  pure $ handleResponse res

post_ :: MonadIO m => FromJSON b => ToJSON a =>  String -> a -> m (MResult b)
post_ url body = liftIO $ do
  request' <- parseRequest $ "POST " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> mailchimpkey]
              $ setRequestBodyJSON body
              request'
  res <- httpLBS request
  pure $ handleResponse res

-- EXPORTED METHODS. TODO I'm hardcoding the "us10" datacenter for now

-- | Retrieves the audiences created for this account.
getLists :: MonadIO m => m (MResult Lists)
getLists = get_ (listsUrl "us10")

-- TODO add user to list

-- | Triggers a journey endpoint sending the email
journeyTrigger :: MonadIO m => JourneyId -> StepId -> EmailAddress -> m (MResult ())
journeyTrigger journeyId stepId email =
  post_ (journeyTriggerUrl "us10" journeyId stepId) $ JourneyTriggerPayload email

-- | Sends an event to mailchimp
addEvent :: MonadIO m => ToJSON a => ListId -> EmailAddress -> EventName -> (Maybe a) -> m (MResult ())
addEvent listId emailAddress eventName maybeBody = do
  let url = addEventUrl "us10" listId emailAddress
      pl = EventPayload eventName maybeBody
  post_ url pl
