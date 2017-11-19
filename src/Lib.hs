{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lib
    ( bot
    , getKillmailJSON
    ) where

-- import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Text hiding (head, replicate)
import qualified Data.ByteString.Lazy as LBS
import Pipes
import Network.Discord
import Control.Monad.IO.Class (liftIO)

import qualified Data.List as L
import Data.IORef
import Data.Aeson (eitherDecode)
import Network.HTTP.Simple

import Filter
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
                && (not . userIsBot $ messageAuthor)) $ do
                    liftIO . putStrLn $ "Getting killmails"
                    forever (getKillmailDiscord msg)

getKillmailJSON :: IO (Either String ApiObject.ApiObject)
getKillmailJSON = do
    request <- parseRequest "https://redisq.zkillboard.com/listen.php"
    response <- httpLBS request
    case eitherDecode (getResponseBody response) of
        Left err -> do
            LBS.writeFile "debug.txt" (getResponseBody response)
            return (Left err)
        json -> return json

getKillmailDiscord :: Message -> Effect DiscordM ()
getKillmailDiscord msg = do
    ekm <- liftIO getKillmailJSON
    case ekm of 
        Left err -> error err
        Right km -> when (afilter km) $ reply msg ((Zkillboard.href . Package.zkb . ApiObject.package) km)

-- amamake
afilter :: Filter
afilter = solarSystemFilter 30002537
-- matchChannel :: IORef (Maybe Channel) -> Snowflake -> IO Bool
-- matchChannel chanRef chanId = do
--     chan <- readIORef chanRef
--     return $ matchChannelMaybe chan chanId

-- matchChannelMaybe :: Maybe Channel -> Snowflake -> Bool
-- matchChannelMaybe chan chanId = maybe False (\c -> chanId == channelId c) chan
