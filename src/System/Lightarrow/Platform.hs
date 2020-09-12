module System.Lightarrow.Platform where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Lazy as LazyW
import qualified Control.Monad.Trans.Writer.Strict as StrictW
import qualified Control.Monad.Trans.State.Lazy as LazyS
import qualified Control.Monad.Trans.State.Strict as StrictS
import           Simulation.Lightarrow.Mode

class Monad m => Platform m where
    data Resources m :: *
    setup   :: m (Resources m)

class (Monad m, Platform a) => MonadPlatform m a where
    liftPlatform :: a b -> m b

liftPF :: MonadPlatform m a => a b -> m b
liftPF = liftPlatform

instance MonadPlatform m a
            => MonadPlatform (LazyS.StateT s m) a where
    liftPlatform = lift . liftPlatform

instance MonadPlatform m a
            => MonadPlatform (StrictS.StateT s m) a where
    liftPlatform = lift . liftPlatform

instance (Monoid s, MonadPlatform m a)
            => MonadPlatform (LazyW.WriterT s m) a where
    liftPlatform = lift . liftPlatform

instance (Monoid s, MonadPlatform m a)
            => MonadPlatform (StrictW.WriterT s m) a where
    liftPlatform = lift . liftPlatform

instance MonadPlatform m a
            => MonadPlatform (ReaderT r m) a where
    liftPlatform = lift . liftPlatform

instance MonadPlatform m a
            => MonadPlatform (ContT r m) a where
    liftPlatform = lift . liftPlatform

instance MonadPlatform m a
            => MonadPlatform (Mode b c m) a where
    liftPlatform = lift . liftPlatform
