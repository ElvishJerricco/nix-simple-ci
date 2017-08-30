{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.TH
import           Data.ByteString (ByteString)
import           Data.Char (toLower)
import           Data.Foldable (for_)
import           Data.List (stripPrefix)
import           Data.Maybe
import           Data.String.Conversions (cs)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Options.Generic
import           Servant
import           Servant.GitHub.Webhook
import           Turtle hiding (stripPrefix)

data Option = Option
  { secret :: ByteString
  , oauth :: ByteString
  , port :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ParseRecord Option

data BuildCommit = BuildCommit
  { _buildCommit_repoName :: Text
  , _buildCommit_repoUrl :: Text
  , _buildCommit_revision :: Text
  }
  deriving (Show, Read, Eq, Ord)

data CommitState
  = CommitState_Pending
  | CommitState_Success
  | CommitState_Failure
  | CommitState_Error
  deriving (Show, Read, Eq, Ord)
deriveJSON defaultOptions { constructorTagModifier = fmap toLower . fromJust . stripPrefix "CommitState_" } ''CommitState

data CommitStatus = CommitStatus
  { _commitStatus_state :: CommitState
  , _commitStatus_target_url :: Maybe Text
  , _commitStatus_description :: Maybe Text
  , _commitStatus_context :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord)
deriveJSON defaultOptions { fieldLabelModifier = fromJust . stripPrefix "_commitStatus_" } ''CommitStatus

type API
  = "webhook"
    :> GitHubEvent '[ 'WebhookPushEvent ]
    :> GitHubSignedReqBody '[JSON] Value
    :> Post '[JSON] ()

main :: IO ()
main = do
  Option {..} <- getRecord "nix-simple-ci"
  mgr         <- newTlsManager
  sem         <- newQSem 1
  let port' = fromMaybe 8080 port
  putStrLn $ "Starting server on " ++ show port'
  withStdoutLogger $ \logger -> do
    let settings = setPort port' $ setLogger logger defaultSettings
    runSettings settings $ serveWithContext (Proxy @API)
                                            (gitHubKey (pure secret) :. EmptyContext)
                                            (server mgr oauth sem)

server :: Manager -> ByteString -> QSem -> Server API
server mgr oauth sem WebhookPushEvent ((), obj) = liftIO $ do
  putStrLn $ "got WebhookPushEvent object: " ++ show obj
  let builds = do
        _buildCommit_repoName <- obj ^? key "repository" . key "full_name" . _String
        _buildCommit_repoUrl  <- obj ^? key "repository" . key "url" . _String
        _buildCommit_revision <- obj ^? key "head_commit" . key "id" . _String
        return BuildCommit
          { _buildCommit_repoName
          , _buildCommit_repoUrl
          , _buildCommit_revision
          }
  putStrLn $ "\n\nbuilding: " ++ show builds
  for_ builds $ \commit ->
    async $ handle @SomeException print $ flip onException (pushError commit) $ do
      success <- bracket_ (waitQSem sem) (signalQSem sem) $ single $ do
        liftIO $ pushStatus mgr oauth commit CommitState_Pending
        code <- build commit
        liftIO $ pushStatus mgr oauth commit code
      case success of
        Just _ -> return ()
        _      -> pushError commit
  where pushError commit = pushStatus mgr oauth commit CommitState_Error
server _ _ _ event _ = liftIO $ putStrLn $ "This shouldn't happen: Unwanted event: " ++ show event

build :: BuildCommit -> Shell CommitState
build BuildCommit { _buildCommit_repoUrl, _buildCommit_revision } = do
  (_, mprefetch) <- liftIO $ procStrict
    "nix-prefetch-git"
    ["--url", _buildCommit_repoUrl, "--rev", _buildCommit_revision, "--fetch-submodules"]
    mzero
  prefetch <- select $   decode @Value $ cs mprefetch
  sha <- select $ prefetch ^? key "sha256" . _String
  let args =
        [ "--option"
        , "use-sandbox"
        , "true"
        , "--no-out-link"
        , "-E"
        , format
          ( "let dl = (import <nixpkgs> {}).fetchgit { fetchSubmodules = true; url = \""
          % s
          % "\"; sha256 = \""
          % s
          % "\"; rev = \""
          % s
          % "\"; }; in import \"${dl}/ci.nix\" {}"
          )
          _buildCommit_repoUrl
          sha
          _buildCommit_revision
        ]
  liftIO $ putStrLn $ T.unpack $ "Running: nix-build " <> T.unwords args
  code <- proc "nix-build" args mzero
  return $ case code of
    ExitSuccess   -> CommitState_Success
    ExitFailure _ -> CommitState_Failure

pushStatus :: Manager -> ByteString -> BuildCommit -> CommitState -> IO ()
pushStatus mgr oauth BuildCommit { _buildCommit_repoName, _buildCommit_revision } state = do
  req <-
    parseUrlThrow
      $  "https://api.github.com/repos/"
      <> T.unpack _buildCommit_repoName
      <> "/statuses/"
      <> T.unpack _buildCommit_revision
  let status = CommitStatus
        { _commitStatus_state       = state
        , _commitStatus_target_url  = Nothing
        , _commitStatus_description = Just $ T.pack $ show state
        , _commitStatus_context     = Just "continuous-integration/nix-simple-ci"
        }
  _ <- httpNoBody
    req
      { method         = "POST"
      , requestHeaders = [ ("Authorization", "token " <> oauth)
                         , ("User-Agent"   , "nix-simple-ci")
                         , ("Accept"       , "application/vnd.github.v3+json")
                         ]
      , requestBody = RequestBodyLBS (encode status)
      }
    mgr
  return ()
