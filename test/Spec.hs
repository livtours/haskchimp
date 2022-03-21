{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Haskchimp
import           Debug.Trace
import Text.Email.Parser (unsafeEmailAddress)
import           Text.Email.Validate (emailAddress)
import qualified Data.ByteString.Char8 as B8
import           Data.Aeson
import           Data.Aeson.Types    (parseFail)
import qualified Data.HashMap.Strict as H

-- | Temporary test key to try stuff out
mailchimpkey :: MailchimpApiKey
mailchimpkey = MailchimpApiKey "87c4a1f0cf1a91107098c27d073dc790-us10"

us10 :: MailchimpDC
us10 = MailchimpDC "us10"

testlist = ListId "fc34064bdc"

member =
         MemberPayload
          (unsafeEmailAddress "branch13" "gmail.com")
          ListMemberStatusUnsubscribed
          EmailTypeHTML
          (H.fromList [ ("FNAME", "DARIO")
                      , ("LNAME", "ODDENINO")
                      , ("FOO", "BANANAS")
                      ])
          False

members = [member]
hotmail = unsafeEmailAddress "branch13" "gmail.com"

memberh =
         MemberPayload
          hotmail
          ListMemberStatusSubscribed
          EmailTypeHTML
          (H.fromList [ ("FNAME", "Dario")
                      , ("LNAME", "Oddenino")
                      , ("ADDRESS", "VIA baba")
                      , ("TEST", "12345")
                      ])
          False

main :: IO ()
main = do
  lists <- getLists mailchimpkey us10
  -- traceShowM lists

  -- res <- listBatchUpdate bfridaylist members

  -- res <- getListMergeFields mailchimpkey us10 testlist
  -- traceShowM res

  -- res <- addUpdateMemberToList mailchimpkey us10 testlist memberh
  -- traceShowM res

  -- v <- journeyTrigger mailchimpkey us10 (JourneyId 3125) (StepId 17225) (unsafeEmailAddress "branch13" "gmail.com")
  -- traceShowM v
  -- res2 <- addEvent travelagent2 hotmail (EventName "apitest")
  --         (H.fromList [ ("PHONE", "12344456") ])

  -- traceShowM res2
  -- case v of
  --   Left s -> traceM $ "PArse error: " <> s
  --   Right (MFail f) -> traceShowM f
  --   Right (MSuccess _) -> traceM "SUCCESS"
  pure ()
