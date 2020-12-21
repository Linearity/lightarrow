
module System.Lightarrow.Load where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import           FRP.BearRiver
import           Simulation.Lightarrow.Task
import           System.Lightarrow.Platform
import           System.IO

-- | Types that identify a certain type of object to load 
type family Location a

-- | Location types that can be converted to and from file paths
class FileLocation a where
    toPath      :: a -> FilePath
    fromPath    :: FilePath -> Maybe a

instance FileLocation FilePath where
    toPath      = id
    fromPath    = Just

-- | Platforms that load objects from a store at run time
class LoadPlatform d m where
    request :: Resources m -> Location d -> m d
    load    :: Resources m -> d -> m d
    unload  :: Resources m -> d -> m d

-- stringFromFile' :: MonadIO m => FilePath -> (String -> IO a) -> m a
-- stringFromFile' path k = liftIO (withFile path ReadMode
--                                     (\handle -> do  !s  <- hGetContents handle
--                                                     !a  <- k s
--                                                     return a))

-- bytestringFromFile' :: MonadIO m => FilePath -> (B.ByteString -> IO a) -> m a
-- bytestringFromFile' path k = liftIO (withFile path ReadMode
--                                         (\handle -> do  !s  <- B.hGetContents handle
--                                                         !b  <- k s
--                                                         return b))

-- | Platforms that save objects in a store at run time
class SavePlatform d m where
    save    :: Resources m -> d -> Location d -> m ()

-- stringToFile :: MonadIO m => String -> FilePath -> m ()
-- stringToFile s path = liftIO (withFile path WriteMode (`hPutStr` s))

-- bytestringToFile :: MonadIO m => B.ByteString -> FilePath -> m ()
-- bytestringToFile b path = liftIO (withFile path WriteMode (`B.hPut` b))

-- | An association list of all entries in an enumeration, each with a
-- corresponding file path made from the entry's name, a directory prefix, and
-- an extension
makePaths :: (Show n, Enum n) =>
                String                  -- ^ directory
                    -> String           -- ^ extension
                    -> [(n, String)]    -- ^ list of enum-path pairs
makePaths dir ext = [(name, prefix ++ show name ++ suffix) | name <- [toEnum 0 ..]]
    where  prefix  = dir ++ "/"
           suffix  = "." ++ ext

-- | A signal function that loads some object and then produces it as constant output
loader :: (MonadPlatform m p, LoadPlatform b p) =>
            Resources p -> Location b -> SF m a b
loader r a = runTask (liftPF (load r =<< request r a)) constant

-- | A signal function that constantly reloads some object, producing it as output
reloader :: (MonadPlatform m a1, LoadPlatform b a1) =>
                Resources a1 -> Location b -> SF m a2 b
reloader r a = runTask (liftPF (request r a)) (constM . liftPF . load r)

-- | Load a collection of names using a mapping of names to locations
loadMany :: (Traversable t, Monad f, LoadPlatform b f) =>
                (a -> Location b)       -- ^ locating function
                    -> Resources f      -- ^ platform resources
                    -> t a              -- ^ initial names
                    -> f (t b)          -- ^ loaded objects
loadMany rename r = traverse (load r <=< request r . rename)