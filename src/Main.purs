module Main where

import Node.Network.SftpClient (list, mkdir, runSftpSession)
import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Effect Unit
main = launchAff_ $ runSftpSession config $ do
  listing1 <- list "/"
  liftEffect $ log $ show listing1
  mkdir "test1" false
  listing2 <- list "/"
  liftEffect $ log $ show listing2

  where
    config =
      { username: "sftpUser"
      , password: "asdqwe123"
      , host: "127.0.0.1"
      , port: "22"}
