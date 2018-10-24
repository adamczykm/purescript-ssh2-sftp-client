module Node.Network.SftpClient.Internal where

import Data.Unit (Unit)
import Effect.Aff.Compat (EffectFnAff)

type Config =
  { host :: String
  , port :: String
  , username :: String
  , password :: String
  }

type FileInfo =
  { type :: String -- file type(-, d, l)
  , name :: String -- file name
  , size :: String -- file size
  , modifyTime :: String -- file timestamp of modified time
  , accessTime :: String -- file timestamp of access time
  , rights ::
    { user :: String
    , group :: String
    , other :: String
    }
  , owner :: String -- user ID
  , group :: String -- group ID
}


foreign import data SftpClientRef :: Type

foreign import unsafeCreateNewClient :: Unit -> SftpClientRef

foreign import connect :: Config -> SftpClientRef -> EffectFnAff Unit

foreign import list :: String -> SftpClientRef -> EffectFnAff (Array FileInfo)

foreign import mkdir :: String -> Boolean -> SftpClientRef -> EffectFnAff Unit

foreign import rmdir :: String -> Boolean -> SftpClientRef -> EffectFnAff Unit
