{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( bot
    ) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Pipes
import Network.Discord

import Data.Aeson (eitherDecode)
import Data.Text hiding (head, replicate)
import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)
import Network.HTTP.Simple
import Text.Megaparsec (parse, parseErrorPretty)

import Esi
import Filter
import FilterParser (filterParser, FilterStatement(Include, Exclude))
import qualified Data.Zkill.ApiObject as ApiObject
import qualified Data.Zkill.Package as Package
import qualified Data.Zkill.Zkillboard as Zkillboard
import qualified Data.Zkill.Killmail as Killmail
import qualified Data.Zkill.Victim as Victim

newtype App a = App {
    _unApp :: ReaderT AppConfig (Effect DiscordM) a
} deriving (Functor, Applicative, Monad, MonadIO)

data AppConfig = AppConfig {
      _botToken :: String
    , _esiPrefix :: String
    , _redisUri :: String
} deriving (Show)

_runApp :: App a -> AppConfig -> Effect DiscordM a
_runApp m cfg = runReaderT (_unApp m) cfg

reply :: Message -> Text -> Maybe Embed -> Effect DiscordM ()
reply Message{messageChannel=chan} cont emb = fetch' $ CreateMessage chan cont emb

killmailReply :: Message -> Package.Package -> Effect DiscordM ()
killmailReply msg pkg = do
    kmContent <- liftIO $ killmailContent pkg
    kmEmbed <- liftIO $ killmailEmbed pkg
    reply msg kmContent (Just kmEmbed)

bot :: String -> IO ()
bot token = do
    runBot (Bot token) $ do
        with ReadyEvent $ \(Init v u _ _ _) -> do
            liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

        with MessageCreateEvent $ \msg@Message{..} -> do
            when ("!zk" `isPrefixOf` messageContent
                && (not . userIsBot $ messageAuthor)) $ case parse filterParser "goofball" (T.drop 3 messageContent) of
                    Left err -> do
                        liftIO . putStrLn $ parseErrorPretty err
                        reply msg ("Parse error: " `append` (pack $ parseErrorPretty err)) Nothing
                    Right (Include parsedFilter) -> do
                        liftIO . putStrLn $ "Getting killmails\n" ++ (unpack messageContent)
                        forever (getKillmailDiscord parsedFilter msg)
                    Right (Exclude _) -> do
                        liftIO . putStrLn $ "Exclude not supported yet\n" ++ (unpack messageContent)
                        reply msg "Exclude not supported yet" Nothing

getKillmailJSON :: IO (Either String ApiObject.ApiObject)
getKillmailJSON = do
    request <- parseRequest "https://redisq.zkillboard.com/listen.php"
    response <- httpLBS request
    case eitherDecode (getResponseBody response) of
        Left err -> do
            LBS.writeFile "debug.txt" (getResponseBody response)
            return (Left err)
        json -> return json

getKillmailDiscord :: Filter -> Message -> Effect DiscordM ()
getKillmailDiscord f msg = do
    ekm <- liftIO getKillmailJSON
    case ekm of
        Left err -> error err
        Right km -> case ApiObject.package km of
            Nothing -> liftIO . putStrLn $ "No Package in ApiObject"
            Just pkg -> when (runFilter f $ pkg) $
                killmailReply msg pkg

killmailContent :: Package.Package -> IO Text
killmailContent pkg = return ((Zkillboard.href . Package.zkb) pkg)

killmailEmbed :: Package.Package -> IO Embed
killmailEmbed pkg = do
    time <- parseTimeM False defaultTimeLocale
        (iso8601DateFormat $ Just "%H:%M:%SZ")
        (unpack . Killmail.killmail_time . Package.killmail $ pkg)
    charName <- case Victim.character_id . Killmail.victim . Package.killmail $ pkg of
        Nothing -> return "No Name"::IO Text
        Just charId -> getCharacterName charId
    return $ Embed {
          embedTitle = "Ship Type"
        , embedType = "rich"::String
        , embedDesc = "Victim: " ++ unpack charName ++ "\nSystem: Location:"
        , embedUrl = zkbUrl (Package.killID pkg)
        , embedTime = time
        , embedColor = 0xf49542
        , embedFields = []
    }

zkbUrl :: Int -> String
zkbUrl killId = zkbUrlBase ++ show killId

zkbUrlBase :: String
zkbUrlBase = "https://zkillboard.com/kill/"