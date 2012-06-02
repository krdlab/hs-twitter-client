{-# LANGUAGE OverloadedStrings #-}
module Hstter.Config where

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BU (fromString)
import Data.ConfigFile
import Data.Either.Utils (forceEither)

readConfig :: FilePath -> IO ConfigParser
readConfig f = do
  conf <- readfile emptyCP { optionxform = id } f
  return $ forceEither conf

-- conf から ByteString 値をとる
forceByteString :: ConfigParser -> OptionSpec -> ByteString
forceByteString cp key = BU.fromString $ forceConf cp key

forceConf :: Get_C a => ConfigParser -> OptionSpec -> a
forceConf cp key = forceEither $ get cp "DEFAULT" key

