{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import System.Environment (getArgs)

import Data.ByteString (ByteString, append)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import qualified Data.ByteString.UTF8 as BU (fromString)
import Data.Text (Text)
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as TE

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types

import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import Network.HTTP.Conduit
import Web.Authenticate.OAuth

--import Control.Exception (SomeException)
--import Control.Exception.Lifted (catch)
--import Prelude hiding (catch)

import Data.ConfigFile
import Data.Either.Utils (forceEither, eitherToMonadError)

main :: IO ()
main = do
  [confFile, op] <- getArgs
  conf <- readfile emptyCP { optionxform = id } confFile
  let cp = forceEither conf
  let oauth = newOAuth { oauthConsumerKey    = forceByteString cp "oauthConsumerKey"
                       , oauthConsumerSecret = forceByteString cp "oauthConsumerSecret" }
  let credential = newCredential (forceByteString cp "accessToken") (forceByteString cp "accessSecret")
  case op of
    "user-stream"     -> userStream oauth credential
    "update-statuses" -> updateStatuses oauth credential
    _                   -> BS.putStrLn $ "unknown op"

-- conf から ByteString 値をとる
forceByteString :: ConfigParser -> OptionSpec -> ByteString
forceByteString cp key = BU.fromString $ forceConf cp key

forceConf :: Get_C a => ConfigParser -> OptionSpec -> a
forceConf cp key = forceEither $ get cp "DEFAULT" key

-- userstream を取得しつづける
userStream :: OAuth -> Credential -> IO ()
userStream oauth credential = do
  withManager $ \manager -> do
    req <- parseUrl "https://userstream.twitter.com/2/user.json"
    signedReq <- signOAuth oauth credential req
    Response {..} <- http signedReq manager
    responseBody C.$$ statusParser success failure

data Status = Status { text :: Text
                     , createdAt :: ByteString
                     , user :: User
                     }

data User = User { screenName :: ByteString
                 }

instance FromJSON Status where
  parseJSON (Object v) = Status
                          <$> v .: "text"
                          <*> v .: "created_at"
                          <*> v .: "user"
  parseJSON _          = mzero

instance FromJSON User where
  parseJSON (Object v) = User
                          <$> v .: "screen_name"
  parseJSON _          = mzero

statusParser :: (Status -> IO ()) -> (String -> IO ()) -> C.Sink ByteString (C.ResourceT IO) ()
statusParser hs hf = do
  j <- CA.sinkParser json -- TODO catch ParseError
  case fromJSON j of
    Success s@(Status {..}) -> liftIO . hs $ s
    Error m                 -> liftIO . hf $ m
  statusParser hs hf

success :: Status -> IO ()
success Status {..} = do
  let User {..} = user
  BS.putStrLn $ "> " `append` screenName `append` " (" `append` createdAt `append` ")"
  TI.putStrLn text
  BS.putStrLn ""

failure :: String -> IO ()
failure m = putStrLn $ "> JSON parse error: " ++ m ++ "\n"

-- 自分のステータスを更新する
updateStatuses :: OAuth -> Credential -> IO ()
updateStatuses oauth credential = do
  status <- BS.putStr "status=" >> TI.getLine
  withManager $ \manager -> do
    baseReq <- parseUrl "https://api.twitter.com/1/statuses/update.json"
    let req = urlEncodedBody [("status", TE.encodeUtf8 status)] baseReq
    signedReq <- signOAuth oauth credential req
    Response {..} <- http signedReq manager
    liftIO . action $ responseStatus
    return ()
  where
    action = putStrLn . show

