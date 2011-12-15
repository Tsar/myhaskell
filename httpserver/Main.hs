import System.IO
import System.Environment (getArgs)
import Network (listenOn, accept, Socket, withSocketsDo, PortID(..))
import Control.Concurrent
import Control.Monad

main = withSocketsDo $ do
    [portStr] <- getArgs
    let port = fromIntegral (read portStr :: Int)
    sock <- listenOn (PortNumber port)
    forever $ do
        (handle, host, portno) <- accept sock
        forkIO $ do
            hSetBuffering handle LineBuffering
            msg <- hGetLine handle
            hPutStr handle msg
            hClose handle
