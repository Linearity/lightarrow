module System.Lightarrow.Platform where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Lazy as LazyW
import qualified Control.Monad.Trans.Writer.Strict as StrictW
import qualified Control.Monad.Trans.State.Lazy as LazyS
import qualified Control.Monad.Trans.State.Strict as StrictS
import           FRP.BearRiver
import           Simulation.Lightarrow.Task

-- | A monad that represents actions that a platform takes, usually input and
-- output
class Monad m => Platform m where
    -- | Resources that the platform acquires and uses
    data Resources m :: *
    -- | Acquire platform resources and return them to the application
    setup   :: m (Resources m)

-- | Monads whose actions can be lifted to platform actions
class (Monad m, Platform a) => MonadPlatform a m where
    liftPlatform :: a b -> m b

-- | A synonym for 'liftPlatform'
liftPF :: MonadPlatform a m => a b -> m b
liftPF = liftPlatform

instance MonadPlatform a m
            => MonadPlatform a (LazyS.StateT s m) where
    liftPlatform = lift . liftPlatform

instance MonadPlatform a m
            => MonadPlatform a (StrictS.StateT s m) where
    liftPlatform = lift . liftPlatform

instance (Monoid s, MonadPlatform a m)
            => MonadPlatform a (LazyW.WriterT s m) where
    liftPlatform = lift . liftPlatform

instance (Monoid s, MonadPlatform a m)
            => MonadPlatform a (StrictW.WriterT s m) where
    liftPlatform = lift . liftPlatform

instance MonadPlatform a m
            => MonadPlatform a (ReaderT r m) where
    liftPlatform = lift . liftPlatform

instance MonadPlatform a m
            => MonadPlatform a (ContT r m) where
    liftPlatform = lift . liftPlatform

instance MonadPlatform a m
            => MonadPlatform a (Task b c m) where
    liftPlatform = lift . liftPlatform
