module Node.Network.SftpClient
  ( module InternalExported
  , SftpSessionM
  , runSftpSession
  , list
  , rmdir
  , mkdir
  , rename
  , delete
  , chmod
  , fastGet
  , fastPut
  ) where

import Control.Alt (class Alt)
import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (*>))
import Control.Bind (class Bind, (=<<))
import Control.Category ((<<<))
import Control.Monad (class Monad)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MonadPlus (class Plus)
import Data.Function (($))
import Data.Functor (class Functor)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup)
import Data.Unit (Unit, unit)
import Effect.Aff (Aff, Error, bracket)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect)
import Node.Network.SftpClient.UnsafeInternal (Config, FileInfo) as InternalExported
import Node.Network.SftpClient.UnsafeInternal (Config, SftpClientRef, FileInfo)
import Node.Network.SftpClient.UnsafeInternal as UnsafeInternal

-- | This type represents a SFTP session.
-- | It hides unsafe internals of underlying JS package.
-- | Each command provided by this library will return a SftpSessionM action
-- | Running SftpSessionM handles both connecting and disconnecting from server
-- | even in case of exception.
newtype SftpSessionM a = SftpSessionM (ReaderT SftpClientRef Aff a)

derive newtype instance functorSftpSessionM ∷ Functor SftpSessionM
derive newtype instance applySftpSessionM ∷ Apply SftpSessionM
derive newtype instance applicativeSftpSessionM ∷ Applicative SftpSessionM
derive newtype instance bindSftpSessionM ∷ Bind SftpSessionM
derive newtype instance monadSftpSessionM ∷ Monad SftpSessionM
derive newtype instance semigroupSftpSessionM ∷ Semigroup a ⇒ Semigroup (SftpSessionM a)
derive newtype instance monoidSftpSessionM ∷ Monoid a ⇒ Monoid (SftpSessionM a)
derive newtype instance altSftpSessionM ∷ Alt SftpSessionM
derive newtype instance plusSftpSessionM ∷ Plus SftpSessionM
derive newtype instance monadEffectSftpSessionM ∷ MonadEffect SftpSessionM
derive newtype instance monadRecSftpSessionM ∷ MonadRec SftpSessionM
derive newtype instance monadAffSftpSessionM ∷ MonadAff SftpSessionM
derive newtype instance monadErrorSftpSessionM ∷ MonadError Error SftpSessionM
derive newtype instance monadThrowSftpSessionM ∷ MonadThrow Error SftpSessionM

-- | A helper for creating SftpSessionM-wrapped library operation.
-- | This function may allow escaping of SftpClientRef and as such is not exported.
unsafeFromRefFnAff ∷ ∀ a. (SftpClientRef → EffectFnAff a) → SftpSessionM a
unsafeFromRefFnAff affFn = SftpSessionM $ lift <<< fromEffectFnAff <<< affFn =<< ask

-- | Assembles SftpSessionM into a single Aff.
-- | Under the hood it makes sure that underlying connection is closed even
-- | in the presence of an exception.
runSftpSession ∷ ∀ a. Config → SftpSessionM a → Aff a
runSftpSession config (SftpSessionM connectedSession) = bracket
  -- acquire resources
  (acquireConnection config)
  -- release resources
  releaseConnection
  -- run session
  (runReaderT connectedSession)

  where
    acquireConnection cfg =
      let ref = UnsafeInternal.unsafeCreateNewClient unit
      in fromEffectFnAff (UnsafeInternal.connect cfg ref) *> pure ref

    releaseConnection ref = fromEffectFnAff (UnsafeInternal.end ref)

-- | Function creating an action executing SFTP command.
-- | List directory given as first argument.
-- | E.g. list "/"j
list ∷ String → SftpSessionM (Array FileInfo)
list = unsafeFromRefFnAff <<< UnsafeInternal.list

-- | Function creating an action executing SFTP command.
-- | Removes directory path from remote location.
rmdir ∷ { path ∷ String, recursive ∷ Boolean} → SftpSessionM Unit
rmdir = unsafeFromRefFnAff <<< UnsafeInternal.rmdir

-- | Function creating an action executing SFTP command.
-- | Creates a remote directory under the given path.
mkdir ∷ { path ∷ String, recursive ∷ Boolean}  → SftpSessionM Unit
mkdir = unsafeFromRefFnAff <<< UnsafeInternal.mkdir

-- | Function creating an action executing SFTP command.
-- | Renames remote location
rename ∷ {from ∷ String, to ∷ String } → SftpSessionM Unit
rename = unsafeFromRefFnAff <<< UnsafeInternal.rename

-- | Function creating an action executing SFTP command.
-- | Deletes file under the path given as a first argument.
delete ∷ String → SftpSessionM Unit
delete = unsafeFromRefFnAff <<< UnsafeInternal.delete

-- | Function creating an action executing SFTP command.
-- | Changing access mode of remote file.
-- | E.g. chmod { dest: "/file.md", mode: "775" }
chmod ∷ { dest ∷ String, mode ∷ String } → SftpSessionM Unit
chmod = unsafeFromRefFnAff <<< UnsafeInternal.chmod

-- | Function creating an action executing SFTP command.
-- | Downloads file from remote path using parallel reads.
fastGet ∷ { remote ∷ String, local ∷ String } → SftpSessionM Unit
fastGet = unsafeFromRefFnAff <<< UnsafeInternal.fastGet

-- | Function creating an action executing SFTP command.
-- | Uploads file under local path using parallel writes.
fastPut ∷ { remote ∷ String, local ∷ String } → SftpSessionM Unit
fastPut = unsafeFromRefFnAff <<< UnsafeInternal.fastPut
