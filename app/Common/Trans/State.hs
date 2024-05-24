{-# LANGUAGE InstanceSigs #-}

module Common.Trans.State where

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ \s -> (\(a, s') -> (f a, s')) <$> sma s

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma = StateT $ \s -> do
    (f, s') <- smab s
    (a, s'') <- sma s'
    return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

newtype Identity a = Identity
  { runIdentity :: a
  }

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return :: a -> Identity a
  return = pure

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity a >>= f = f a

type State s a = StateT s Identity a

get :: (Monad m) => StateT s m s
get = StateT $ \s -> return (s, s)

put :: (Monad m) => s -> StateT s m ()
put state = StateT $ \_ -> return ((), state)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = do
  state <- get
  put $ f state

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

runState :: State s a -> (s -> (a, s))
runState (StateT sa) s = runIdentity $ sa s

evalState :: State s a -> s -> a
evalState m s = fst $ runState m s
