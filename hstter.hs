{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import System.Environment (getArgs)
import System.IO (stdout)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.Monoid
import Network.HTTP.Conduit
import Web.Authenticate.OAuth

import qualified System.IO.UTF8 as U
import Data.ByteString.UTF8
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Attoparsec as AP

import qualified Data.Conduit.Attoparsec as CA

import Control.Exception (SomeException)
import Control.Exception.Lifted (catch)
import Prelude hiding (catch)

import Data.ConfigFile
import Data.Either.Utils (forceEither, eitherToMonadError)

data Status = Status { text :: Text
                     , createdAt :: String
                     , user :: User
                     } deriving (Show)

data User = User { screenName :: String
                 } deriving (Show)

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

-- とれなかったら error で
forceConf :: Get_C a => ConfigParser -> OptionSpec -> a
forceConf cp key = forceEither $ get cp "DEFAULT" key

forceByteString :: ConfigParser -> OptionSpec -> ByteString
forceByteString cp key = fromString $ forceConf cp key

main :: IO ()
main = do
  [confFile, op] <- getArgs
  conf <- readfile emptyCP { optionxform = id } confFile
  let cp = forceEither conf
  let oauth = newOAuth { oauthConsumerKey    = forceByteString cp "oauthConsumerKey"
                       , oauthConsumerSecret = forceByteString cp "oauthConsumerSecret" }
  let credential = newCredential (forceByteString cp "accessToken") (forceByteString cp "accessSecret")
  case op of
    "--user-stream"     -> userStream oauth credential
    "--update-statuses" -> updateStatuses oauth credential
    _                   -> BS.putStrLn $ "unknown op"

userStream :: OAuth -> Credential -> IO ()
userStream oauth credential = do
  withManager $ \manager -> do
    req <- parseUrl "https://userstream.twitter.com/2/user.json"
    signedReq <- signOAuth oauth credential req
    Response {..} <- http signedReq manager
    responseBody C.$$ statusParser success failure

statusParser :: (Status -> IO ()) -> (String -> IO ()) -> C.Sink ByteString (C.ResourceT IO) ()
statusParser hs hf = do
  j <- CA.sinkParser json -- TODO catch ParseError
  case fromJSON j of
    Success s@(Status {..}) -> liftIO . hs $ s
    Error m                 -> liftIO . hf $ m
  statusParser hs hf

success :: Status -> IO ()
success s = U.putStrLn . show $ s

failure :: String -> IO ()
failure m = U.putStrLn $ "JSON parse error: " ++ m

updateStatuses :: OAuth -> Credential -> IO ()
updateStatuses oauth credential = do
  BS.putStr "status="
  status <- BS.getLine
  withManager $ \manager -> do
    baseReq <- parseUrl "https://api.twitter.com/1/statuses/update.json"
    let req = urlEncodedBody [("status", status)] baseReq
    signedReq <- signOAuth oauth credential req
    response <- http signedReq manager
    responseBody response C.$$ CB.sinkHandle stdout

