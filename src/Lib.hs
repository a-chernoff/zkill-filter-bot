{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lib
    ( bot
    , getKillmailJSON
    ) where

-- import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Text as T
import Data.Text hiding (head, replicate)
import qualified Data.ByteString.Lazy as LBS
import Pipes
import Network.Discord
import Control.Monad.IO.Class (liftIO)

import qualified Data.List as L
import Data.IORef
import Data.Aeson (eitherDecode)
import Network.HTTP.Simple
import Text.Megaparsec (parse, parseErrorPretty)

import Filter
import FilterParser (filterParser, FilterStatement(Include, Exclude))
import qualified Data.Zkill.ApiObject as ApiObject
import qualified Data.Zkill.Package as Package
import qualified Data.Zkill.Zkillboard as Zkillboard

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

bot :: String -> IO ()
bot token = do
    cidRef <- newIORef Nothing
    runBot (Bot token) $ do
        with ReadyEvent $ \(Init v u chans _ _) -> do
            liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u
            liftIO . writeIORef cidRef $ L.find (\a -> case a of
                Text _ _ name _ _ _ _ -> name == "Zkill-Bot"
                _ -> False) chans

        with MessageCreateEvent $ \msg@Message{..} -> do
            -- cid <- liftIO (readIORef cidRef)
            -- liftIO . putStrLn $ (show cid)
            -- chanMatch <- liftIO $ matchChannel cidRef messageChannel
            when ("!zk" `isPrefixOf` messageContent
                && (not . userIsBot $ messageAuthor)) $ case parse filterParser "goofball" (T.drop 3 messageContent) of
                    Left err -> do
                        liftIO . putStrLn $ parseErrorPretty err
                        reply msg ("Parse error: " `append` (pack $ parseErrorPretty err))
                    Right (Include parsedFilter) -> do
                        liftIO . putStrLn $ "Getting killmails\n" ++ (unpack messageContent)
                        forever (getKillmailDiscord parsedFilter msg)
                    Right (Exclude _) -> do
                        liftIO . putStrLn $ "Exclude not supported yet\n" ++ (unpack messageContent)
                        reply msg "Exclude not supported yet"

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
                reply msg ((Zkillboard.href . Package.zkb) pkg)

-- matchChannel :: IORef (Maybe Channel) -> Snowflake -> IO Bool
-- matchChannel chanRef chanId = do
--     chan <- readIORef chanRef
--     return $ matchChannelMaybe chan chanId

-- matchChannelMaybe :: Maybe Channel -> Snowflake -> Bool
-- matchChannelMaybe chan chanId = maybe False (\c -> chanId == channelId c) chan
