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

bfridaylist = ListId "4252578848"

main :: IO ()
main = do
  -- lists <- getLists
  -- traceShowM lists
  let member =
         MemberPayload
          (unsafeEmailAddress "branch13" "gmail.com")
          ListMemberStatusUnsubscribed
          EmailTypeHTML
          (H.fromList [ ("FNAME", "DARIO")
                     , ("LNAME", "ODDENINO")
                     ])
          False

  -- let members = [member]
  -- res <- listBatchUpdate bfridaylist members
  -- res <- addMemberToList bfridaylist member
  -- traceShowM res
  -- v <- journeyTrigger (JourneyId 2569) (StepId 13689) (unsafeEmailAddress "branch13" "gmail.com")
  -- case v of
  --   Left s -> traceM $ "PArse error: " <> s
  --   Right (MFail f) -> traceShowM f
  --   Right (MSuccess _) -> traceM "SUCCESS"
  pure ()
