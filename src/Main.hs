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
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Servant
import Servant.GitHub.Webhook
import System.Environment
import Turtle

main :: IO ()
main = do
  [secret] <- getArgs
  sem      <- newQSem 1
  putStrLn "Starting server on 8080"
  run 8080 $ serveWithContext (Proxy @API)
                              (gitHubKey (pure $ BS.pack secret) :. EmptyContext)
                              (server sem)

server :: QSem -> Server API
server sem WebhookPushEvent ((), obj) = liftIO $ do
  putStrLn $ "got WebhookPushEvent object: " ++ show obj
  let builds = do
        repo <- obj ^.. key "repository" . key "full_name" . _String
        rev  <- obj ^.. key "commits" . values . key "id" . _String
        return (repo, rev)
  traverse_ (async . bracket_ (waitQSem sem) (signalQSem sem) . sh . uncurry build) builds
server _ event _ = liftIO $ putStrLn $ "This shouldn't happen: Unwanted event: " ++ show event

type API
  = "webhook"
    :> GitHubEvent '[ 'WebhookPushEvent ]
    :> GitHubSignedReqBody '[JSON] Value
    :> Post '[JSON] ()

build :: Text -> Text -> Shell ExitCode
build repo rev = do
  let url = "https://github.com/" <> repo <> "/archive/" <> rev <> ".tar.gz"
  sha <- inproc "nix-prefetch-url" ["--unpack", url] mzero
  proc
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
