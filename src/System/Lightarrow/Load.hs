
module System.Lightarrow.Load where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import           FRP.BearRiver
import           Simulation.Lightarrow.Mode
import           System.Lightarrow.Platform
import           System.IO

type family Location a

class FileLocation a where
    toPath      :: a -> FilePath
    fromPath    :: FilePath -> Maybe a

instance FileLocation FilePath where
    toPath      = id
    fromPath    = Just

class LoadPlatform d m where
    request :: Resources m -> Location d -> m d
    load    :: Resources m -> d -> m d
    unload  :: Resources m -> d -> m d

stringFromFile' path k = liftIO (withFile path ReadMode
                                    (\handle -> do  !s  <- hGetContents handle
                                                    !a  <- k s
                                                    return a))

bytestringFromFile' path k = liftIO (withFile path ReadMode
                                        (\handle -> do  !s  <- B.hGetContents handle
                                                        !b  <- k s
                                                        return b))

class SavePlatform d m where
    save    :: Resources m -> d -> Location d -> m ()

stringToFile s path = liftIO (withFile path WriteMode (`hPutStr` s))

bytestringToFile b path = liftIO (withFile path WriteMode (`B.hPut` b))
{-

Commonly names are enumeration types and locations are strings representing
paths in a file system. We can presume to find named prior data in the file
system listed by the constructors of their names, appended with a type-specific
extension.

-}
makePaths :: (Show n, Enum n)
                =>  String -> String -> [(n, String)]
makePaths dir ext = [(name, prefix ++ show name ++ suffix) | name <- [toEnum 0 ..]]
    where  prefix  = dir ++ "/"
           suffix  = "." ++ ext

loader r a = runMode (liftPF (load r =<< request r a)) constant

reloader r a = runMode (liftPF (request r a)) (constM . liftPF . load r)

loadMany rename r = traverse (load r <=< request r . rename)