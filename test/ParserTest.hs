{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Zkill.Package
import Data.Zkill.Victim
import Filter
import FilterParser
import Text.Megaparsec
import Data.Maybe

main :: IO ()
main = runTestTT tests >> return ()

tests :: Test
tests = TestList [
      TestLabel "Symbol Parser" testSymbol
    , TestLabel "Parens Parser" testParens
    , TestLabel "Quote Parser" testQuotedLit
    , TestLabel "Character Parser" testCharacterStmt
    , TestLabel "Filter Parser" testFilterStatement
    ]

testSymbol :: Test
testSymbol = TestCase (assertEqual
    "for symbol parser"
    (Just "testSymbol")
    (parseMaybe (symbol "testSymbol") "testSymbol"))

testParens :: Test
testParens = TestCase (assertEqual
    "for parens parser"
    (Just "testSymbol")
    (parseMaybe (parens $ symbol "testSymbol") "( testSymbol  ) "))

testQuotedLit :: Test
testQuotedLit = TestCase (assertEqual
    "for quotes parser"
    (Just "testSymbol")
    (parseMaybe quotedLit "\"testSymbol\""))

testCharacterStmt :: Test
testCharacterStmt = TestCase (assertBool
    "for character literal"
    (isJust ((parseMaybe characterStmt "1234")::Maybe ((Package -> Victim) -> Filter))))

testFilterStatement :: Test
testFilterStatement = TestCase (assertBool
    "for include statement"
    (maybe False isInclude $ parseMaybe filterParser "include attacker corp 1234"))

isInclude :: FilterStatement -> Bool
isInclude (Include _) = True
isInclude _ = False
