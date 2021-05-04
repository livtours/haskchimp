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

prbloggers = ListId "4252578848"
travelagent2 = ListId "3efe0bde3c"
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
hotmail = unsafeEmailAddress "budgnopjrywqfokpld" "miucce.com"

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
  -- lists <- getLists
  -- traceShowM lists

  -- res <- listBatchUpdate bfridaylist members

  -- res <- getListMergeFields travelagent2
  -- traceShowM res

  -- res <- addUpdateMemberToList travelagent2 memberh
  -- traceShowM res

  -- v <- journeyTrigger (JourneyId 2653) (StepId 14013) (unsafeEmailAddress "branch13" "gmail.com")
  -- traceShowM v
  -- res2 <- addEvent travelagent2 hotmail (EventName "apitest")
  --         (H.fromList [ ("PHONE", "12344456") ])

  -- traceShowM res2
  -- case v of
  --   Left s -> traceM $ "PArse error: " <> s
  --   Right (MFail f) -> traceShowM f
  --   Right (MSuccess _) -> traceM "SUCCESS"
  pure ()
