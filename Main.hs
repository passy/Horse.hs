{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TI
import qualified Data.MarkovChain as MC

import System.Random (RandomGen, getStdGen)
import System.Environment (getArgs)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (ResumableSource)
import Network.HTTP.Conduit (parseUrl, withManager, http, urlEncodedBody, Request, Response)
import Web.Authenticate.OAuth (OAuth(..), Credential, newOAuth, newCredential, signOAuth)

statusesUrl :: String
statusesUrl = "https://api.twitter.com/1.1/statuses/update.json"

main :: IO ()
main = do
    [confFile] <- getArgs
    conf <- Conf.load [Conf.Required confFile]
    oauth <- makeOAuth conf
    cred <- makeCredential conf
    rnd <- getStdGen

    input <- TI.getLine
    let tweet = T.pack $ generateTweet rnd (T.unpack input)
    -- That's ugly ...
    _ <- postTweet oauth cred tweet
    return ()

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


postTweet :: OAuth -> Credential -> T.Text -> IO (Response (ResumableSource (ResourceT IO) ByteString))
postTweet oauth cred tweet = do
    let params = [("status", TE.encodeUtf8 tweet)]
    req <- makeRequest statusesUrl params
    -- Error handling?
    executeOAuthRequest oauth cred req

makeRequest :: MonadThrow m => String -> [(ByteString, ByteString)] -> m Request
makeRequest url params = do
    req <- parseUrl url
    return $ urlEncodedBody params req

executeOAuthRequest :: OAuth -> Credential -> Request -> IO (Response (ResumableSource (ResourceT IO) ByteString))
executeOAuthRequest oauth cred request =
    withManager $ \manager -> do
        signed <- signOAuth oauth cred request
        http signed manager

generateTweet :: (Ord a, RandomGen g) => g -> [a] -> [a]
generateTweet rnd input =
    take 140 $ MC.run 4 input 0 rnd
