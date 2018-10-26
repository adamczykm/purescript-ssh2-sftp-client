module Node.Network.SftpClient.UnsafeInternal where

import Data.Unit (Unit)
import Effect.Aff.Compat (EffectFnAff)

type Config =
  { host ∷ String
  , port ∷ String
  , username ∷ String
  , password ∷ String
  }

type FileInfo =
  { type ∷ String -- file type(-, d, l)
  , name ∷ String -- file name
  , size ∷ Int -- file size
  , modifyTime ∷ Int -- file timestamp of modified time
  , accessTime ∷ Int -- file timestamp of access time
  , rights ∷
    { user ∷ String
    , group ∷ String
    , other ∷ String
    }
  , owner ∷ Int -- user ID
  , group ∷ Int -- group ID
}


foreign import data SftpClientRef ∷ Type

foreign import unsafeCreateNewClient ∷ Unit → SftpClientRef

foreign import connect ∷ Config → SftpClientRef → EffectFnAff Unit

foreign import end ∷ SftpClientRef → EffectFnAff Unit

foreign import list ∷ String → SftpClientRef → EffectFnAff (Array FileInfo)

foreign import mkdir ∷ {path ∷ String, recursive ∷ Boolean} → SftpClientRef → EffectFnAff Unit

foreign import rmdir ∷ {path ∷ String, recursive ∷ Boolean} → SftpClientRef → EffectFnAff Unit

foreign import rename ∷ {from ∷ String, to ∷ String } → SftpClientRef → EffectFnAff Unit

foreign import delete ∷ String → SftpClientRef → EffectFnAff Unit

foreign import chmod ∷ { dest ∷ String, mode ∷ String }→ SftpClientRef → EffectFnAff Unit

foreign import fastGet ∷ {remote ∷ String, local ∷ String } → SftpClientRef → EffectFnAff Unit

foreign import fastPut ∷ {local ∷ String, remote ∷ String } → SftpClientRef → EffectFnAff Unit
