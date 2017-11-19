{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack, unpack)
import Data.Ini.Config
import Lib

main :: IO ()
main = do
    cfgFile <- readFile "config.ini"
    case parseIniFile (pack cfgFile) iniParser of
        Left err -> putStrLn err
        Right cfg -> putStrLn (unpack cfg) >> bot (unpack cfg)

iniParser :: IniParser Text
iniParser = section "BOT" (field "token")