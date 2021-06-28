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
  , addMemberToList
  , getListMember
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
import           Haskchimp.Types
import           Network.HTTP.Simple
import           Text.Email.Parser          (EmailAddress, toByteString)
import qualified Data.HashMap.Strict   as H

-- dc is us10 for us
rootUrl :: MailchimpDC -> String
rootUrl (MailchimpDC dc) = "https://" <> dc <> ".api.mailchimp.com/3.0/"

listsUrl :: MailchimpDC -> String
listsUrl dc = rootUrl dc <> "lists/"

listMergeFieldsUrl :: MailchimpDC -> ListId -> String
listMergeFieldsUrl dc (ListId l) = rootUrl dc <> "lists/" <> unpack l <> "/merge-fields"

addMemberToListUrl :: MailchimpDC -> ListId -> String
addMemberToListUrl dc (ListId l) = rootUrl dc <> "lists/" <> unpack l <> "/members"

listMemberUrl :: MailchimpDC -> ListId -> EmailAddress -> String
listMemberUrl dc (ListId l) emailAddress =
  let hashedMail = show $ md5 $ L8.fromStrict $ toByteString emailAddress
  in rootUrl dc <> "lists/" <> unpack l <> "/members/" <> hashedMail

-- listBatchUpdateUrl :: String -> ListId -> String
-- listBatchUpdateUrl dc (ListId l) = rootUrl dc <> "lists/" <> unpack l

journeyTriggerUrl :: MailchimpDC -> JourneyId -> StepId -> String
journeyTriggerUrl dc (JourneyId j) (StepId s) =
  rootUrl dc <> "customer-journeys/journeys/" <> show j <> "/steps/" <> show s <> "/actions/trigger"

addEventUrl :: MailchimpDC -> ListId -> EmailAddress -> String
addEventUrl dc (ListId l) emailAddress =
  let hashedMail = show $ md5 $ L8.fromStrict $ toByteString emailAddress
  in rootUrl dc <> "lists/" <> unpack l <> "/members/" <> hashedMail <> "/events"


handleResponse :: FromJSON a => Response L8.ByteString -> MResult a
handleResponse r =
  if getResponseStatusCode r >= 400
  then MFail $ eitherDecode $ getResponseBody r
  else case eitherDecode (getResponseBody r) of
    Left s -> MFail $ Left s
    Right b -> MSuccess b

-- | Helper method for get requests
get_ :: MonadIO m => FromJSON a => MailchimpApiKey -> String -> m (MResult a)
get_ (MailchimpApiKey ak) url = liftIO $ do
  request' <- parseRequest $ "GET " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> ak]
                                 request'
  res <- httpLBS request
  pure $ handleResponse res

-- | Post helper to avoid repetition
postHelper_ :: MonadIO m => ToJSON a => MailchimpApiKey -> String -> a -> m (Response L8.ByteString)
postHelper_ (MailchimpApiKey ak) url body = liftIO $ do
  request' <- parseRequest $ "POST " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> ak]
              $ setRequestBodyJSON body
              request'
  httpLBS request

-- | A post request expecting a json response
post_ :: MonadIO m => FromJSON b => ToJSON a => MailchimpApiKey -> String -> a -> m (MResult b)
post_ mailchimpkey url body = liftIO $ do
  res <- postHelper_ mailchimpkey url body
  pure $ handleResponse res

-- | A post request expecting no body
postUnit_ :: MonadIO m => ToJSON a => MailchimpApiKey -> String -> a -> m (MResult ())
postUnit_ mailchimpkey url body = liftIO $ do
  res <- postHelper_ mailchimpkey url body
  if getResponseStatusCode res >= 400
  then pure $ MFail $ eitherDecode (getResponseBody res)
  else pure $ MSuccess ()


put_ :: MonadIO m => FromJSON b => ToJSON a => MailchimpApiKey -> String -> a -> m (MResult b)
put_ (MailchimpApiKey ak) url body = liftIO $ do
  request' <- parseRequest $ "PUT " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> ak]
              $ setRequestBodyJSON body
              request'
  res <- httpLBS request
  pure $ handleResponse res

-- EXPORTED METHODS. TODO I'm hardcoding the "us10" datacenter for now

noMergeVars :: MergeVars
noMergeVars = H.empty

-- | Retrieves the audiences created for this account.
getLists :: MonadIO m => MailchimpApiKey -> MailchimpDC -> m (MResult Lists)
getLists mailchimpkey dc = get_ mailchimpkey (listsUrl dc)

getListMergeFields :: MonadIO m => MailchimpApiKey -> MailchimpDC -> ListId -> m (MResult Object)
getListMergeFields mailchimpkey dc listId = do
  get_ mailchimpkey $ listMergeFieldsUrl dc listId <> "/3"

getListMember :: MonadIO m => MailchimpApiKey -> MailchimpDC -> ListId -> EmailAddress -> m (MResult ListMemberResult)
getListMember mailchimpkey dc listId email = do
  let url = listMemberUrl dc listId email
  get_ mailchimpkey url

-- | Adds or update a member to a mailchimp list.
-- | The required merge fields has to be passed or the request will fail.
addUpdateMemberToList :: MonadIO m => MailchimpApiKey -> MailchimpDC -> ListId -> MemberPayload -> m (MResult ListMemberResult)
addUpdateMemberToList mailchimpkey dc listId member@(MemberPayload email _ _ _ _) = do
  let url = listMemberUrl dc listId email
  put_ mailchimpkey url member

-- | Adds a member to a mailchimp list.
addMemberToList :: MonadIO m => MailchimpApiKey -> MailchimpDC -> ListId -> MemberPayload -> m (MResult ListMemberResult)
addMemberToList mailchimpkey dc listId member = do
  let url = addMemberToListUrl dc listId
  post_ mailchimpkey url member

-- | Sends a batch request, similar to addMemberToList
-- listBatchUpdate :: MonadIO m => ListId -> [MemberPayload] -> m (MResult ListBatchUpdateResponse)
-- listBatchUpdate listId membs = do
--   let url = listBatchUpdateUrl "us10" listId
--   post_ url $ ListBatchUpdatePayload membs False

-- | Triggers a journey endpoint sending the email
journeyTrigger :: MonadIO m => MailchimpApiKey -> MailchimpDC -> JourneyId -> StepId -> EmailAddress -> m (MResult ())
journeyTrigger mailchimpkey dc journeyId stepId email =
  postUnit_ mailchimpkey (journeyTriggerUrl dc journeyId stepId) $ JourneyTriggerPayload email

-- | Sends an event to mailchimp
addEvent :: MonadIO m => MailchimpApiKey -> MailchimpDC -> ListId -> EmailAddress -> EventName -> MergeVars -> m (MResult ())
addEvent mailchimpkey dc  listId emailAddress eventName body = do
  let url = addEventUrl dc listId emailAddress
      pl = EventPayload eventName body
  postUnit_ mailchimpkey url pl
