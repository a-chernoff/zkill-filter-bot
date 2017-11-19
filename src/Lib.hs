{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lib
    ( bot
    , getKillmailJSON
    , getKillmailJSON2
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, sequence_)
import Data.Text hiding (head, replicate)
import qualified Data.ByteString.Lazy as LBS
import Pipes
import Network.Discord
import Control.Monad.IO.Class (liftIO)

import Data.Aeson                  (parseJSON, eitherDecode)
import Network.HTTP.Simple

import qualified Data.Zkill.ApiObject as ApiObject
import qualified Data.Zkill.Package as Package
import qualified Data.Zkill.Zkillboard as Zkillboard

sendToChannel :: Snowflake -> Text -> Effect DiscordM ()
sendToChannel chan cont = fetch' $ CreateMessage chan cont Nothing

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan (append cont $ pack (show chan)) Nothing

bot :: String -> IO ()
bot clientSecret = runBot (Bot clientSecret) $ do
    with ReadyEvent $ \(Init v u _ _ _) ->
        liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

    with MessageCreateEvent $ \msg@Message{..} -> do
        when ("Ping" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $
            getKillmailDiscord

getKillmailJSON :: IO (ApiObject.ApiObject)
getKillmailJSON = do
    request <- parseRequest "https://redisq.zkillboard.com/listen.php"
    -- response <- httpJSON request
    response <- httpJSONEither request
    -- return (getResponseBody response)
    case getResponseBody response of
        Left err -> error (show err)
        Right json -> return json

getKillmailJSON2 :: IO (Either String ApiObject.ApiObject)
getKillmailJSON2 = do
    request <- parseRequest "https://redisq.zkillboard.com/listen.php"
    response <- httpLBS request
    return (eitherDecode (getResponseBody response))

getKillmailDiscord :: Effect DiscordM ()
getKillmailDiscord = do
    ekm <- liftIO (getKillmailJSON2)
    case ekm of 
        Left err -> error err
        Right km -> sendToChannel 0
            ((Zkillboard.href . Package.zkb . ApiObject.package) km)

            