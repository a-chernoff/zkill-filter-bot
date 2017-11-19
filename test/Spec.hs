{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

main :: IO ()
main = runTestTT tests >> return ()

tests :: Test
tests = TestList [
      -- TestLabel "Alliance Parser" testAllianceParser
    -- , TestLabel "Killmail Parser" testKillmailParser
    ]
