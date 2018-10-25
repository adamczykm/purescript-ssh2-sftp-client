module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (catchError, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Network.SftpClient (fastGet, fastPut, list, runSftpSession)

main :: Effect Unit
main = launchAff_ $ flip catchError
  onError $
  runSftpSession config $ do
    listing1 <- list "/"
    liftEffect $ log $ show listing1
    fastPut { local: "./README.md", remote: "/root/README.md" }
    listing2 <- list "/root/"
    liftEffect $ log $ show listing2
    fastGet { local: "./README2.md", remote: "/root/README.md" }

  where
    onError e = liftEffect $ log $ "Catched error: " <> (show e)
    config =
      { username: "foo"
      , password: "asdqwe123"
      , host: "127.0.0.1"
      , port: "22"}
