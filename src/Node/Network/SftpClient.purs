module Node.Network.SftpClient where

import Control.Alt (class Alt)
import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (*>))
import Control.Bind (class Bind, bind, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad (class Monad)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT(..), ask, lift, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MonadPlus (class Plus)
import Data.Function (($))
import Data.Functor (class Functor)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup)
import Data.Unit (Unit, unit)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect)
import Node.Network.SftpClient.Internal (Config, SftpClientRef, FileInfo)
import Node.Network.SftpClient.Internal as Internal

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

fromRefFnAff ∷ ∀ a. (SftpClientRef → EffectFnAff a) → SftpSessionM a
fromRefFnAff affFn = SftpSessionM $ lift <<< fromEffectFnAff <<< affFn =<< ask

runSftpSession ∷ forall a. Config → SftpSessionM a → Aff a
runSftpSession config session =
  let (SftpSessionM connectedSession) = connect config *> session
  in runReaderT connectedSession (Internal.unsafeCreateNewClient unit)

connect ∷ Config → SftpSessionM Unit
connect config = fromRefFnAff $ Internal.connect config

list ∷ String → SftpSessionM (Array FileInfo)
list directory = fromRefFnAff $ Internal.list directory

rmdir ∷ String → Boolean → SftpSessionM Unit
rmdir path recursive = fromRefFnAff $ Internal.rmdir path recursive

mkdir ∷ String → Boolean → SftpSessionM Unit
mkdir path recursive = fromRefFnAff $ Internal.mkdir path recursive
