import System.IO
import System.Environment (getArgs)
import Network (listenOn, accept, Socket, withSocketsDo, PortID(..))
import Control.Concurrent
import Control.Monad

fuckMe = do
    putStrLn "fuck me"

makeResponseForRequest :: String -> IO String
makeResponseForRequest request = do
    if (take 5 request) == "GET /"
        then do
            fileName <- return ((drop 5 (take ((length request) - 10) request)))
            fileHandle <- catch (openFile fileName ReadMode) (\_ -> do
                fuckMe
                openFile fileName ReadMode)
            fileContents <- hGetContents fileHandle
            hClose fileHandle
            return ("HTTP/1.1 200 OK\r\n\r\n" ++ fileContents)
        else return "HTTP/1.1 400 Bad Request\r\n\r\n"

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
            (makeResponseForRequest request) >>= (hPutStr handle)
            (makeResponseForRequest request) >>= putStrLn  --DEBUG CODE
            hClose handle
