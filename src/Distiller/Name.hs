{-# LANGUAGE UndecidableInstances #-}

module Distiller.Name where

import Distiller.Prelude
import System.Console.Repline


data Name = MkName !ShortByteString !Int
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Hashable)

instance Clone Name where
  cloneC (MkName label _) = (>>=) $ fresh label

instance Fresh Name where
  freshC x = (>>=) $ MkName x <$> freshInt


newtype Var = VarName { varName :: Name }
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (Hashable, Clone, Fresh)

newtype Fun = FunName { funName :: Name }
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (Hashable, Clone, Fresh)

newtype Con = ConName { conName :: Name }
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (Hashable, Clone, Fresh)

type Bndr = Var


class Monad m => MonadFresh m where
  freshInt :: m Int

  default freshInt :: (m ~ t n, MonadTrans t, MonadFresh n) => m Int
  freshInt = lift freshInt

instance MonadFresh m => MonadFresh (ListT m)
instance MonadFresh m => MonadFresh (StateT s m)
instance MonadFresh m => MonadFresh (WriterT s m)
instance MonadFresh m => MonadFresh (ReaderT s m)
instance MonadFresh m => MonadFresh (ExceptT e m)
instance MonadFresh m => MonadFresh (ContT e m)
instance MonadFresh m => MonadFresh (HaskelineT m)


class Fresh a where
  freshC :: MonadFresh m => ShortByteString -> (a -> m b) -> m b

fresh :: (MonadFresh m, Fresh a) => ShortByteString -> m a
fresh = freshC ?? pure


class Clone a where
  cloneC :: MonadFresh m => a -> (a -> m b) -> m b

clone :: (MonadFresh m, Clone a) => a -> m a
clone = cloneC ?? pure


type FreshM = FreshT Identity

newtype FreshT m a = FreshT { unFreshT :: StateT Int m a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadFix, MonadPlus, MonadTrans)
  deriving newtype (MonadReader r, MonadWriter w, MonadError e, MonadCont, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance Monad m => MonadFresh (FreshT m) where
  freshInt = FreshT $ state $ id &&& (+ 1)

instance MonadState s m => MonadState s (FreshT m) where
  get = lift get
  put = lift . put


evalFreshT :: Monad m => FreshT m a -> m a
evalFreshT = flip evalStateT 1 . unFreshT

evalFresh :: FreshM a -> a
evalFresh = flip evalState 1 . unFreshT
