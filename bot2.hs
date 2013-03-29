import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch)

server = "irc.freenode.org"
port   = 6667
chan   = "#lolbots"
nick   = "jesusbot"
owner  = "shadowdaemon"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

-- Set up actions to run on start and end, and run the main loop.
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catchIOError (runReaderT run st) (const $ return ())

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch

-- Connect to the server and return the initial bot state.
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully.
-- Join a channel, and start processing commands.
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :user")
    write "JOIN" chan
    asks socket >>= listen

-- Process each line from the server (this needs flood prevention somewhere).
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (words s)
  where
    forever a = a >> forever a
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- Get the actual message.
getMsg :: [String] -> [String]
getMsg a
    | length (intersect a ["PRIVMSG"]) > 0 = b
    | otherwise = []
  where
    b = (drop 1 (a!!3)) : (drop 4 a)

-- Who is speaking to us?
getNick :: [String] -> String
--getNick []         = []
--getNick a = (drop 1 (dropWhile (/= '!') (head a)))
getNick = drop 1 . takeWhile (/= '!') . head

-- Are we being spoken to?
spokenTo :: [String] -> Bool
spokenTo []              = False
spokenTo a
    | b == nick          = True
    | b == (nick ++ ":") = True
    | otherwise          = False
  where
    b = (head a)

-- Is this a private message?
--spokenPrivate

-- Figure shit out.
eval :: [String] -> Net ()
eval a
    | length msg == 0         = return ()
    | spokenTo msg == True    = evalMsg1 who (tail msg)
    | otherwise               = evalMsg2 who msg
--    | otherwise               = return ()
  where
    msg = getMsg a
    who = getNick a

-- Respond to message addressed to us.
evalMsg1 :: String -> [String] -> ReaderT Bot IO ()
evalMsg1 _ []                               = return () -- Ignore because not PRIVMSG.
evalMsg1 _ b | isPrefixOf "!id " (head b)   = chanMsg (drop 4 (head b))
evalMsg1 a b | isPrefixOf "!" (head b)      = if a == owner then evalCmd b else return ()
evalMsg1 a b                                = replyMsg a $ reverse $ unwords b

-- Respond to message.
evalMsg2 :: String -> [String] -> ReaderT Bot IO ()
evalMsg2 _ []                               = return () -- Ignore because not PRIVMSG.
evalMsg2 _ b | isPrefixOf "!id " (head b)   = chanMsg (drop 4 (head b))
evalMsg2 _ ["lol"]                          = chanMsg "lol"
evalMsg2 _ b                                = if length (intersect b ["jesus"]) > 0 || length (intersect b ["Jesus"]) > 0 then chanMsg "Jesus!" else return ()
--evalMsg2 _ _                                = return ()

-- Dispatch a command for owner.
evalCmd :: [String] -> ReaderT Bot IO ()
evalCmd (x:xs) | x == "!quit"              = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)

-- Send a message to the channel.
chanMsg :: String -> Net ()
chanMsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a reply message.
replyMsg :: String -> String -> Net ()
replyMsg rnick msg = write "PRIVMSG" (chan ++ " :" ++ rnick ++ ": " ++ msg)

-- Send a private message.
privMsg :: String -> String -> Net ()
privMsg rnick msg = write "PRIVMSG" (rnick ++ " :" ++ msg)

-- Send a message out to the server we're currently connected to.
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
 
-- Convenience.
io :: IO a -> Net a
io = liftIO
