{- |
Copyright: (c) 2021 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskchimp
  ( getLists
  , getListMergeFields
  , addUpdateMemberToList
  , listBatchUpdate
  , journeyTrigger
  , addEvent
  , noMergeVars
  -- TODO export real types
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
import           Text.Email.Parser          (EmailAddress, toByteString)
import qualified Data.HashMap.Strict   as H

-- dc is us10 for us
rootUrl :: String -> String
rootUrl dc = "https://" <> dc <> ".api.mailchimp.com/3.0/"

-- campaignsUrl :: String -> String
-- campaignsUrl dc = rootUrl dc <> "campaigns/"

listsUrl :: String -> String
listsUrl dc = rootUrl dc <> "lists/"

listMergeFieldsUrl :: String -> ListId -> String
listMergeFieldsUrl dc (ListId l) = rootUrl dc <> "lists/" <> unpack l <> "/merge-fields"

addMemberToListUrl :: String -> ListId -> String
addMemberToListUrl dc (ListId l) = rootUrl dc <> "lists/" <> unpack l <> "/members"

addUpdateMemberToListUrl :: String -> ListId -> EmailAddress -> String
addUpdateMemberToListUrl dc (ListId l) emailAddress =
  let hashedMail = show $ md5 $ L8.fromStrict $ toByteString emailAddress
  in rootUrl dc <> "lists/" <> unpack l <> "/members/" <> hashedMail

listBatchUpdateUrl :: String -> ListId -> String
listBatchUpdateUrl dc (ListId l) = rootUrl dc <> "lists/" <> unpack l

journeyTriggerUrl :: String -> JourneyId -> StepId -> String
journeyTriggerUrl dc (JourneyId j) (StepId s) =
  rootUrl dc <> "customer-journeys/journeys/" <> show j <> "/steps/" <> show s <> "/actions/trigger"

addEventUrl :: String -> ListId -> EmailAddress -> String
addEventUrl dc (ListId l) emailAddress =
  let hashedMail = show $ md5 $ L8.fromStrict $ toByteString emailAddress
  in rootUrl dc <> "lists/" <> unpack l <> "/members/" <> hashedMail <> "/events"

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

-- | Post helper to avoid repetition
postHelper_ :: MonadIO m => ToJSON a => String -> a -> m (Response L8.ByteString)
postHelper_ url body = liftIO $ do
  request' <- parseRequest $ "POST " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> mailchimpkey]
              $ setRequestBodyJSON body
              request'
  httpLBS request

-- | A post request expecting a json response
post_ :: MonadIO m => FromJSON b => ToJSON a =>  String -> a -> m (MResult b)
post_ url body = liftIO $ do
  res <- postHelper_ url body
  pure $ handleResponse res

-- | A post request expecting no body
postUnit_ :: MonadIO m => ToJSON a => String -> a -> m (MResult ())
postUnit_ url body = liftIO $ do
  res <- postHelper_ url body
  if getResponseStatusCode res >= 400
  then pure $ MFail <$> eitherDecode (getResponseBody res)
  else pure $ Right $ MSuccess ()


put_ :: MonadIO m => FromJSON b => ToJSON a => String -> a -> m (MResult b)
put_ url body = liftIO $ do
  request' <- parseRequest $ "PUT " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> mailchimpkey]
              $ setRequestBodyJSON body
              request'
  res <- httpLBS request
  pure $ handleResponse res

-- EXPORTED METHODS. TODO I'm hardcoding the "us10" datacenter for now

noMergeVars :: MergeVars
noMergeVars = H.empty

-- | Retrieves the audiences created for this account.
getLists :: MonadIO m => m (MResult Lists)
getLists = get_ (listsUrl "us10")

getListMergeFields :: MonadIO m => ListId -> m (MResult Object)
getListMergeFields listId = do
  get_ $ listMergeFieldsUrl "us10" listId <> "/3"

-- | Adds or update a member to a mailchimp list.
-- | The required merge fields has to be passed or the request will fail.
addUpdateMemberToList :: MonadIO m => ListId -> MemberPayload -> m (MResult ListMemberResult)
addUpdateMemberToList listId member@(MemberPayload email _ _ _ _) = do
  let url = addUpdateMemberToListUrl "us10" listId email
  put_ url member

-- | Sends a batch request, similar to addMemberToList
listBatchUpdate :: MonadIO m => ListId -> [MemberPayload] -> m (MResult ListBatchUpdateResponse)
listBatchUpdate listId membs = do
  let url = listBatchUpdateUrl "us10" listId
  post_ url $ ListBatchUpdatePayload membs False

-- | Triggers a journey endpoint sending the email
journeyTrigger :: MonadIO m => JourneyId -> StepId -> EmailAddress -> m (MResult ())
journeyTrigger journeyId stepId email =
  postUnit_ (journeyTriggerUrl "us10" journeyId stepId) $ JourneyTriggerPayload email

-- | Sends an event to mailchimp
addEvent :: MonadIO m => ListId -> EmailAddress -> EventName -> MergeVars -> m (MResult ())
addEvent listId emailAddress eventName body = do
  let url = addEventUrl "us10" listId emailAddress
      pl = EventPayload eventName body
  postUnit_ url pl
