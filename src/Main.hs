{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (stripPrefix)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import Servant
import Servant.GitHub.Webhook
import System.Environment
import Turtle hiding (stripPrefix)

data BuildCommit = BuildCommit
  { _buildCommit_repo :: Text
  , _buildCommit_revision :: Text
  }
  deriving (Show, Read, Eq, Ord)
makeLenses ''BuildCommit

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
makeLenses ''CommitStatus
deriveJSON defaultOptions { fieldLabelModifier = fromJust . stripPrefix "_commitStatus_" } ''CommitStatus

type API
  = "webhook"
    :> GitHubEvent '[ 'WebhookPushEvent ]
    :> GitHubSignedReqBody '[JSON] Value
    :> Post '[JSON] ()

main :: IO ()
main = do
  [secret, oauth] <- getArgs
  mgr             <- newTlsManager
  sem             <- newQSem 1
  putStrLn "Starting server on 8080"
  run 8080 $ serveWithContext (Proxy @API)
                              (gitHubKey (pure $ BS.pack secret) :. EmptyContext)
                              (server mgr (BS.pack oauth) sem)

server :: Manager -> ByteString -> QSem -> Server API
server mgr oauth sem WebhookPushEvent ((), obj) = liftIO $ do
  putStrLn $ "got WebhookPushEvent object: " ++ show obj
  let builds = do
        repo <- obj ^.. key "repository" . key "full_name" . _String
        rev  <- obj ^.. key "commits" . values . key "id" . _String
        return BuildCommit
          { _buildCommit_repo     = repo
          , _buildCommit_revision = rev
          }
  for_ builds $ \commit -> async $ do
    success <- try @SomeException $ bracket_ (waitQSem sem) (signalQSem sem) $ single $ do
      liftIO $ pushStatus mgr oauth commit CommitState_Pending
      code <- build commit
      liftIO $ pushStatus mgr oauth commit code
    case success of
      Right (Just _) -> return ()
      _              -> () <$ try @SomeException (pushStatus mgr oauth commit CommitState_Error)
server _ _ _ event _ = liftIO $ putStrLn $ "This shouldn't happen: Unwanted event: " ++ show event

build :: BuildCommit -> Shell CommitState
build commit = do
  let url =
        "https://github.com/"
          <> _buildCommit_repo commit
          <> "/archive/"
          <> _buildCommit_revision commit
          <> ".tar.gz"
  sha  <- inproc "nix-prefetch-url" ["--unpack", url] mzero
  code <- proc
    "nix-build"
    [ "--option"
    , "use-sandbox"
    , "true"
    , "-E"
    , format
      ( "let dl = (import <nixpkgs> {}).fetchzip { url = \""
      % s
      % "\"; sha256 = \""
      % l
      % "\"; }; in import \"${dl}/ci.nix\" {}"
      )
      url
      sha
    ]
    mzero
  return $ case code of
    ExitSuccess   -> CommitState_Success
    ExitFailure _ -> CommitState_Failure

-- https://api.github.com/repos/octocat/Hello-World/statuses/6dcb09b5b57875f334f61aebed695e2e4193db5e
pushStatus :: Manager -> ByteString -> BuildCommit -> CommitState -> IO ()
pushStatus mgr oauth commit state = do
  req <-
    parseUrlThrow
      $  "https://api.github.com/repos/"
      <> T.unpack (_buildCommit_repo commit)
      <> "/statuses/"
      <> T.unpack (_buildCommit_revision commit)
  let status = CommitStatus
        { _commitStatus_state       = state
        , _commitStatus_target_url  = Nothing
        , _commitStatus_description = Just $ T.pack $ show state
        , _commitStatus_context     = Just "continuous-integration/nix-simple-ci"
        }
  _ <- httpNoBody
    req { method         = "POST"
        , requestHeaders = [("Authentication", oauth)]
        , requestBody    = RequestBodyLBS (encode status)
        }
    mgr
  return ()
