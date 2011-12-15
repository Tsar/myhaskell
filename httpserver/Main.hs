import System.IO
import System.Environment (getArgs)
import Network (listenOn, accept, Socket, withSocketsDo, PortID(..))
import Control.Concurrent
import Control.Monad

makeResponseForRequest :: String -> String
makeResponseForRequest request = if (take 5 request) == "GET /" then "HTTP/1.1 200 OK\r\n\r\ntrololo" else "HTTP/1.1 404 Not Found\r\n\r\n"

main :: IO ()
main = withSocketsDo $ do
    [portStr] <- getArgs
    let port = fromIntegral (read portStr :: Int)
    sock <- listenOn (PortNumber port)
    forever $ do
        (handle, _, _) <- accept sock
        forkIO $ do
            hSetBuffering handle LineBuffering
            request <- hGetLine handle
            putStrLn request   --DEBUG CODE
            hSetBuffering handle NoBuffering
            hPutStr handle (makeResponseForRequest request)
            putStrLn (makeResponseForRequest request)  --DEBUG CODE
            hClose handle
