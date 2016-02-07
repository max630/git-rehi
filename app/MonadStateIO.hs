{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module MonadStateIO (evalStateT,execStateT) where

import Data.Tuple (swap)

import qualified Control.Applicative as App
import qualified Control.Monad.Catch as MC
import qualified Control.Monad.Trans as MT
import qualified Control.Monad.Trans.State as MTS
import qualified Control.Monad.Trans.Reader as MTR
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.State as MS
import qualified Control.Monad.IO.Class as MIO
import qualified Data.IORef as R

newtype RefTag s = RefTag { getRef :: R.IORef s }

type IOStateT s m a = App.WrappedMonad (MTR.ReaderT (RefTag s) m) a

instance MIO.MonadIO m => MS.MonadIO (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) where
  liftIO f = App.WrapMonad (MIO.liftIO f)

instance MIO.MonadIO m => MS.MonadState s (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) where
  get = App.WrapMonad (MR.ask >>= (MIO.liftIO . R.readIORef . getRef))
  put val = App.WrapMonad (MR.ask >>= (MIO.liftIO . (`R.writeIORef` val) . getRef))
  state func = App.WrapMonad (MR.ask >>= (MIO.liftIO . (\ref -> R.atomicModifyIORef ref (swap . func)) . getRef))

instance MR.MonadReader r m => MR.MonadReader r (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) where
  ask = App.WrapMonad (MT.lift MR.ask)
  reader f = App.WrapMonad (MT.lift $ MR.reader f)
  local f (App.WrapMonad (MTR.ReaderT sub)) = App.WrapMonad (MTR.ReaderT (\v -> MR.local f (sub v)))

instance MC.MonadThrow m => MC.MonadThrow (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) where
  throwM ex = App.WrapMonad (MC.throwM ex)

instance MC.MonadCatch m => MC.MonadCatch (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) where
  catch (App.WrapMonad body) handler = App.WrapMonad impl
   where
    impl = MC.catch
              body
              (App.unwrapMonad . handler)

instance MC.MonadMask m => MC.MonadMask (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) where
  mask mkBody0 = App.WrapMonad (MC.mask (\rest -> App.unwrapMonad (mkBody0 $ w rest)))
    where
      w :: ((MTR.ReaderT (RefTag s) m) a -> (MTR.ReaderT (RefTag s) m) a) -> (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) a -> (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) a
      w rest = App.WrapMonad . rest . App.unwrapMonad
  uninterruptibleMask mkBody0 = App.WrapMonad (MC.uninterruptibleMask (\rest -> App.unwrapMonad (mkBody0 $ w rest)))
    where
      w :: ((MTR.ReaderT (RefTag s) m) a -> (MTR.ReaderT (RefTag s) m) a) -> (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) a -> (App.WrappedMonad (MTR.ReaderT (RefTag s) m)) a
      w rest = App.WrapMonad . rest . App.unwrapMonad

runIOStateT :: MIO.MonadIO m => IOStateT s m a -> s -> m (R.IORef s, a)
runIOStateT (App.WrapMonad func) initS = do
  ref <- MIO.liftIO $ R.newIORef initS
  result <- MTR.runReaderT func (RefTag ref)
  pure (ref,result)

evalStateT :: MIO.MonadIO m => IOStateT s m a -> s -> m a
evalStateT func initS = fmap snd (runIOStateT func initS)

execStateT :: MIO.MonadIO m => IOStateT s m a -> s -> m s
execStateT func initS = do
  (ref,_) <- runIOStateT func initS
  MIO.liftIO $ R.readIORef ref
