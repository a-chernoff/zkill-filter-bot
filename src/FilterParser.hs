{-# LANGUAGE OverloadedStrings #-}
module FilterParser where

import Filter
import Control.Applicative as A
import Data.Text as T hiding (concat)
import Data.Void
import Data.Zkill.Package
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data FilterStatement = Include Filter | Exclude Filter
type Parser = Parsec Void Text

instance Show FilterStatement where
    show (Include _) = "Include"
    show (Exclude _) = "Exclude"

-- lexer

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "///" "///")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

quotedLit :: Parser Text
quotedLit = do
    _ <- char '"'
    strings <- many (notChar '"')
    _ <- char '"'
    return $ T.pack strings
-- character :: Parser Text
-- character = nonEscape <|> escape

-- escape :: Parser Text
-- escape = do
--     d <- char "\\"
--     c <- oneOf "\\\"0nrvtbf"
--     return $ T.concat [d, c]

-- nonEscape :: Parser Text
-- nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

-- parser

filterParser :: Parser FilterStatement
filterParser = between sc eof filterParser'
    where
        filterParser' = includeStmt <|> excludeStmt

includeStmt :: Parser FilterStatement
includeStmt = do
    _ <- symbol "include"
    a <- filterStmt
    return $ Include a

excludeStmt :: Parser FilterStatement
excludeStmt = do
    _ <- symbol "exclude"
    a <- filterStmt
    return $ Exclude a

filterStmt :: Parser Filter
filterStmt = s2Stmt
    <|> notStmt
    <|> try andStmt
    <|> orStmt
    -- <|> parens filterStmt
    -- <|> s2Stmt

s2Stmt :: Parser Filter
s2Stmt = attackerStmt
    <|> victimStmt
    <|> systemStmt
    <|> constellationStmt
    <|> regionStmt
    <|> npcStmt

s3Stmt :: Character a => Parser ((Package -> a) -> Filter)
s3Stmt = corpStmt
    <|> allianceStmt
    <|> shipStmt
    -- <|> weaponStmt
    <|> characterStmt

andStmt :: Parser Filter
andStmt = do
    a <- filterStmt
    _ <- symbol "and"
    b <- filterStmt
    return (a .& b)

orStmt :: Parser Filter
orStmt = do
    a <- filterStmt
    _ <- symbol "or"
    b <- filterStmt
    return (a .| b)

notStmt :: Parser Filter
notStmt = do
    _ <- symbol "not"
    a <- filterStmt
    return $ notFilter a

attackerStmt :: Parser Filter
attackerStmt = do
    _ <- symbol "attacker"
    f <- s3Stmt
    return $ attackerFilter f

victimStmt :: Parser Filter
victimStmt = do
    _ <- symbol "victim"
    f <- s3Stmt
    return $ victimFilter f

systemStmt :: Parser Filter
systemStmt = do
    _ <- symbol "system"
    i <- integer
    return $ solarSystemFilter i

constellationStmt :: Parser Filter
constellationStmt = do
    _ <- symbol "constellation"
    i <- integer
    return $ constellationFilter i

regionStmt :: Parser Filter
regionStmt = do
    _ <- symbol "region"
    i <- integer
    return $ regionFilter i

npcStmt :: Parser Filter
npcStmt = do
    _ <- symbol "is"
    _ <- symbol "npc"
    return $ isNpcFilter

corpStmt :: Character a => Parser ((Package -> a) -> Filter)
corpStmt = do
    _ <- symbol "corp"
    i <- integer
    return $ corporationFilter i

allianceStmt :: Character a => Parser ((Package -> a) -> Filter)
allianceStmt = do
    _ <- symbol "alliance"
    i <- integer
    return $ allianceFilter i

shipStmt :: Character a => Parser ((Package -> a) -> Filter)
shipStmt = do
    _ <- symbol "ship"
    i <- integer
    return $ shipTypeFilter i

characterStmt :: Character a => Parser ((Package -> a) -> Filter)
characterStmt = do
    -- _ <- symbol "char"
    i <- integer
    return $ characterFilter i
