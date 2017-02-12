{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.WebAPI
    ( SlackConfig(..)
    , makeSlackCall

      -- * Methods
    , rtm_start
    , chat_postMessage
    , files_upload
    ) where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wreq as W
import Web.Slack.Types

-- | Configuration options needed to connect to the Slack API
data SlackConfig = SlackConfig
   { _slackApiToken :: String -- ^ API Token for Bot
   } deriving (Show)

makeLenses ''SlackConfig

makeSlackCall
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> String
    -> Maybe W.Part
    -> (W.Options -> W.Options)
    -> m Value
makeSlackCall conf method postData setArgs = do
    let url = "https://slack.com/api/" ++ method
    let setToken = W.param "token" .~ [T.pack (conf ^. slackApiToken)]
    let opts = W.defaults & setToken & setArgs
    let request = maybe (W.getWith opts url) (W.postWith opts url) postData
    rawResp <- liftIO request
    resp <- rawResp ^? W.responseBody . _Value ?? "Couldn't parse response"
    case resp ^? key "ok"  . _Bool of
        Just True  -> return resp
        Just False -> throwError $ resp ^. key "error" . _String
        Nothing    -> throwError "Couldn't parse key 'ok' from response"

-------------------------------------------------------------------------------
-- Methods

-- See https://api.slack.com/methods for the docs.

rtm_start
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> m (T.Text, SlackSession)
rtm_start conf = do
    resp <- makeSlackCall conf "rtm.start" Nothing id
    url <- resp ^? key "url" . _String ?? "rtm_start: No url!"
    sessionInfo <- fromJSON' resp
    return (url, sessionInfo)

chat_postMessage
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> ChannelId
    -> T.Text
    -> [Attachment]
    -> m ()
chat_postMessage conf (Id cid) msg as =
    void $ makeSlackCall conf "chat.postMessage" Nothing $
        (W.param "channel"     .~ [cid]) .
        (W.param "text"        .~ [msg]) .
        (W.param "attachments" .~ [encode' as]) .
        (W.param "as_user"     .~ ["true"])

files_upload
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> ChannelId
    -> FilePath
    -> T.Text
    -> m File
files_upload conf (Id cid) path name = do
    let file = W.partFile "file" path
    resp <- makeSlackCall conf "files.upload" (Just file) $
        (W.param "channels" .~ [cid]) .
        (W.param "filename" .~ [name]) .
        (W.param "as_user"  .~ ["true"])
    fileInfo <- fromJSON' resp
    return fileInfo

-------------------------------------------------------------------------------
-- Helpers

encode' :: ToJSON a => a -> T.Text
encode' = T.decodeUtf8 . BL.toStrict . encode

fromJSON' :: (FromJSON a, MonadError T.Text m) => Value -> m a
fromJSON' x = case fromJSON x of
    Error e -> throwError (T.pack e)
    Success r -> return r

-- | Like '(??)' from Control.Error, but a bit more general and with the
-- right precedence.
infixl 7 ??
(??) :: MonadError e m => Maybe a -> e -> m a
x ?? e = maybe (throwError e) return x

