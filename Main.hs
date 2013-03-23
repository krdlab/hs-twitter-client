{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import System.Environment                   (getArgs)
import System.Locale                        (defaultTimeLocale)
import Data.Time                            (parseTime, getCurrentTimeZone, utcToZonedTime)
import Data.ByteString                      (ByteString)
import Data.ByteString.Char8                ()
import qualified Data.ByteString.Char8      as BC
import qualified Data.Text.IO               as TI
import qualified Data.Text.Encoding         as TE
import qualified Data.Conduit               as C
import qualified Data.Conduit.Attoparsec    as CA
import Network.HTTP.Conduit                 (parseUrl, withManager, http, urlEncodedBody, Response(..))
import Web.Authenticate.OAuth               (OAuth(..), Credential(..), newOAuth, newCredential, signOAuth)
import Control.Monad.IO.Class               (liftIO)
import Data.Aeson                           (json, fromJSON, Result(..))
import qualified Data.Configurator          as Conf
import Hstter.Type

twitterUserStreamUrl, twitterUpdateStatusesUrl :: String
twitterUserStreamUrl     = "https://userstream.twitter.com/2/user.json"
twitterUpdateStatusesUrl = "https://api.twitter.com/1.1/statuses/update.json"

main :: IO ()
main = do
  [confFile, op] <- getArgs
  conf  <- Conf.load [Conf.Required confFile]
  oauth <- makeOAuth conf
  cred  <- makeCredential conf
  case op of
    "user-stream"     -> userStream oauth cred
    "update-statuses" -> updateStatuses oauth cred
    _                 -> BC.putStrLn "usage: hstter [user-stream | update-statuses]"
  where
    makeOAuth conf = do
      key    <- Conf.lookupDefault "" conf "oauthConsumerKey"
      secret <- Conf.lookupDefault "" conf "oauthConsumerSecret"
      return $ newOAuth
        { oauthConsumerKey    = key
        , oauthConsumerSecret = secret
        }
    makeCredential conf = do
      token  <- Conf.lookupDefault "" conf "accessToken"
      secret <- Conf.lookupDefault "" conf "accessSecret"
      return $ newCredential token secret

-- userstream を取得しつづける
userStream :: OAuth -> Credential -> IO ()
userStream oauth credential = do
  req <- parseUrl twitterUserStreamUrl
  withManager $ \manager -> do
    signed <- signOAuth oauth credential req
    res    <- http signed manager
    responseBody res C.$$+- statusParser success failure
  where
    success Status {..} =
      case parseCreatedAt createdAt of
        Just ctime -> printStatus user ctime
        Nothing    -> putStrLn "> time parse error: created_at"
      where
        parseCreatedAt = parseTime defaultTimeLocale "%a %b %d %H:%M:%S %z %Y" . BC.unpack
        printStatus (User {..}) ctime = do
          tzone <- getCurrentTimeZone
          BC.putStrLn $ BC.concat ["> ", screenName, ": ", name, " (", BC.pack . show $ utcToZonedTime tzone ctime, ")"]
          TI.putStrLn text
          BC.putStrLn $ BC.concat ["(", idStr, ")"]
          BC.putStrLn ""
    failure m = putStrLn $ "> JSON parse error: " ++ m ++ "\n"

-- -- parser
statusParser :: (Status -> IO ()) -> (String -> IO ()) -> C.Sink ByteString (C.ResourceT IO) ()
statusParser hs hf = do
  j <- CA.sinkParser json
  liftIO $ case fromJSON j of
    Success s -> hs s
    Error   m -> hf m
  statusParser hs hf

-- 自分のステータスを更新する
updateStatuses :: OAuth -> Credential -> IO ()
updateStatuses oauth credential = do
  status <- inputStatus
  req    <- makeRequest twitterUpdateStatusesUrl status
  withManager $ \manager -> do
    signed <- signOAuth oauth credential req
    res    <- http signed manager
    printResponse res
  where
    inputStatus = BC.putStr "status=" >> TI.getLine
    makeRequest url status = do
      req <- parseUrl url
      return $ urlEncodedBody
        [ ("status", TE.encodeUtf8 status)
        ] req
    printResponse = liftIO . print . responseStatus

