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
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.ByteString.Char8  (pack)
import           Haskchimp.Types
import           Network.HTTP.Simple
import           Text.Email.Parser      (EmailAddress)
import Debug.Trace

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

-- | Temporary test key to try stuff out
mailchimpkey :: String
mailchimpkey = "87c4a1f0cf1a91107098c27d073dc790-us10"

handleResponse :: FromJSON a => Response L8.ByteString -> Either String (MResponse a)
handleResponse r = if getResponseStatusCode r >= 400
                   then MFail <$> (eitherDecode $ getResponseBody r)
                   else MSuccess <$> (eitherDecode $ getResponseBody r)

-- | Helper method for get requests
get_ :: MonadIO m => FromJSON a => String -> m (Either String (MResponse a))
get_ url = liftIO $ do
  request' <- parseRequest $ "GET " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> mailchimpkey]
                                 request'
  res <- httpLBS request
  pure $ handleResponse res

post_ :: MonadIO m => Show b => FromJSON b => ToJSON a =>  String -> a -> m (Either String (MResponse b))
post_ url body = liftIO $ do
  request' <- parseRequest $ "POST " <> url
  let request = setRequestHeader "Authorization"
                                 [pack $ "Bearer " <> mailchimpkey]
              $ setRequestBodyJSON body
              request'
  res <- httpLBS request
  pure $ handleResponse res

-- | Retrieves the audiences created for this account.
getLists :: MonadIO m => m (Either String (MResponse Lists))
getLists = get_ (listsUrl "us10")


journeyTrigger :: MonadIO m => JourneyId -> StepId -> EmailAddress -> m (Either String (MResponse ()))
journeyTrigger journeyId stepId email = do
  post_ (journeyTriggerUrl "us10" journeyId stepId) $ JourneyTriggerPayload email
