{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Haskchimp
import           Debug.Trace
import           Text.Email.Parser


main :: IO ()
main = do
  lists <- getLists
  traceShowM lists
  -- v <- journeyTrigger (JourneyId 2569) (StepId 13689) (unsafeEmailAddress "branch13" "gmail.com")
  -- case v of
  --   Left s -> traceM $ "PArse error: " <> s
  --   Right (MFail f) -> traceShowM f
  --   Right (MSuccess _) -> traceM "SUCCESS"
