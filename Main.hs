{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Prelude

import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString)
import Network.HTTP.Conduit (parseUrl, withManager, http, urlEncodedBody,
                             Response(..), Request)
import Web.Authenticate.OAuth (OAuth(..), Credential(..), newOAuth,
                               newCredential, signOAuth)

statusesUrl :: String
statusesUrl = "https://api.twitter.com/1.1/statuses/update.json"

main :: IO ()
main = do
    [confFile] <- getArgs
    conf <- Conf.load [Conf.Required confFile]
    oauth <- makeOAuth conf
    cred <- makeCredential conf

    postTweet oauth cred "Hello, Horse"

    where
        makeOAuth conf = do
            key <- Conf.lookupDefault "" conf "oauthConsumerKey"
            secret <- Conf.lookupDefault "" conf "oauthConsumerSecret"
            return $ newOAuth {
                oauthConsumerKey = key,
                oauthConsumerSecret = secret
            }
        makeCredential conf = do
            token <- Conf.lookupDefault "" conf "accessToken"
            secret <- Conf.lookupDefault "" conf "accessSecret"
            return $ newCredential token secret


postTweet :: OAuth -> Credential -> T.Text -> IO ()
postTweet oauth cred tweet = do
    let params = [("status", TE.encodeUtf8 tweet)]
    req <- makeRequest statusesUrl params
    resp <- executeOAuthRequest oauth cred req
    putStrLn "Yo"

makeRequest :: MonadThrow m => String -> [(ByteString, ByteString)] -> m Request
makeRequest url params = do
    req <- parseUrl url
    return $ urlEncodedBody params req

-- Arghhh, what's the type?!
-- executeOAuthRequest :: MonadThrow m => OAuth -> Credential -> m Request
executeOAuthRequest oauth cred request = do
    liftIO $ withManager $ \manager -> do
        signed <- signOAuth oauth cred request
        http signed manager
